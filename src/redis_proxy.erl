-module(redis_proxy).
-include_lib("kernel/include/logger.hrl").
%% API
-export([
  start_link/1,
  start_link/2,
  stop/0,
  stop/1,
  status/0,
  status/1
]).

-export([
  q/1,
  q/2,
  q/3,
  qp/1,
  qp/2,
  qp/3,
  foreach_redis/4,
  foreach_redis/5
]).

%% 利用redis自旋锁
-export([trans/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_POOL_SIZE, 16).
-define(TIMEOUT, 5000).
-define(REDIS_LOCK_TIME, 5).
-define(REDIS_TIMEOUT, 5000).
-define(TRANS_SLEEP_TIME_MAX, 100).

start_link(Args) ->
  start_link(?SERVER, Args).

start_link(Name, Args) when is_atom(Name), is_map(Args) ->
  Config = #{worker_module => redis_worker, name => {local, Name}, size => ?DEFAULT_POOL_SIZE},
  M = maps:merge(Config, Args),
  poolboy:start_link(maps:to_list(M));
start_link(Name, Args) when is_list(Args) ->
  start_link(Name, maps:from_list(Args)).

stop() ->
  stop(?SERVER).

stop(Name) when is_atom(Name) ->
  poolboy:stop(Name).

status() ->
  status(?SERVER).

status(Name) when is_atom(Name) ->
  poolboy:status(Name).

q(Command) -> q(Command, ?TIMEOUT).

q(Command, Timeout) -> q(?SERVER, Command, Timeout).

q(Name, Command, Timeout) ->
  poolboy:transaction(Name, fun(PID) -> eredis:q(PID, Command) end, Timeout).

qp(Command) -> qp(Command, ?TIMEOUT).

qp(Command, Timeout) -> qp(?SERVER, Command, Timeout).

qp(Name, Command, Timeout) ->
  poolboy:transaction(Name, fun(PID) -> eredis:qp(PID, Command) end, Timeout).

foreach_redis(REG, Fun, CntPerTime, Timeout) ->
  foreach_redis(?SERVER, REG, Fun, CntPerTime, Timeout).

foreach_redis(Name, REG, Fun, CntPerTime, Timeout) ->
  foreach_redis(Name, <<"0">>, 0, REG, CntPerTime, Timeout, Fun).

foreach_redis(_Name, <<"0">>, Times, _REG, _CNT, _Timeout, _Fun) when Times > 0 ->
  ok;
foreach_redis(Name, Cur, Times, REG, CNT, Timeout, Fun) ->
  CMD = case REG == "*" of
          true -> ["SCAN", Cur, "COUNT", erlang:integer_to_list(CNT)];
          false -> ["SCAN", Cur, "MATCH", REG, "COUNT", erlang:integer_to_list(CNT)]
        end,
  case q(Name, CMD, Timeout) of
    {ok, [Cur2, []]} ->
      foreach_redis(Name, Cur2, Times + 1, REG, CNT, Timeout, Fun);
    {ok, [Cur2, L]} ->
      case Fun of
        {M, F} -> M:F(L);
        _ -> Fun(L)
      end,
      foreach_redis(Name, Cur2, Times + 1, REG, CNT, Timeout, Fun)
  end.

%% ====================
%% Fun = {M,F,A} | fun/0 | {fun/x,Args}
trans(LockName, Fun) when is_binary(LockName) ->
  RandBin = erlang:term_to_binary(erlang:make_ref()),
  trans_i(LockName, RandBin, ?REDIS_LOCK_TIME, Fun).

trans_i(LockKey, RandBin, LockTime, Fun) ->
  case redis_proxy:q(["SET", LockKey, RandBin, "EX", erlang:integer_to_list(LockTime), "NX"], ?REDIS_TIMEOUT) of
    {ok, <<"OK">>} ->
      Ret = case Fun of
              {M, F, A} -> catch apply(M, F, A);
              _ when is_function(Fun, 0) -> catch Fun();
              {F, A} when is_function(F) -> catch erlang:apply(F, A)
            end,
      case redis_proxy:q(["GET", LockKey], ?REDIS_TIMEOUT) of
        {ok, RandBin} -> redis_proxy:q(["DEL", LockKey], ?REDIS_TIMEOUT);
        _ -> pass
      end,
      Ret;
    {ok, undefined} ->
      sleep_random(?TRANS_SLEEP_TIME_MAX),
      trans_i(LockKey, RandBin, LockTime, Fun);
    {error, Msg} ->
      ?LOG_ERROR("trans_error ~ts ~p", [LockKey, Msg]),
      sleep_random(?TRANS_SLEEP_TIME_MAX),
      trans_i(LockKey, RandBin, LockTime, Fun)
  end.

sleep_random(Max) ->
  Sleep = rand:uniform(Max),
  timer:sleep(Sleep).