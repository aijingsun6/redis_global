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
-export([
  trans/2,
  trans/3,
  trans/5
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_POOL_SIZE, 16).
-define(TIMEOUT, 5000).
-define(REDIS_LOCK_TIME, 5000).
-define(REDIS_TRANS_TIMEOUT, infinity).
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
%% @return {ok, Result} | {error,timeout}
trans(LockName, Fun) when is_binary(LockName) ->
  trans(?SERVER, LockName, Fun, ?REDIS_LOCK_TIME, ?REDIS_TRANS_TIMEOUT).

trans(Name, LockName, Fun) when is_binary(LockName) ->
  trans(Name, LockName, Fun, ?REDIS_LOCK_TIME, ?REDIS_TRANS_TIMEOUT).

trans(Name, LockName, Fun, LockTime, Timeout) when Timeout =:= infinity ->
  RandBin = erlang:term_to_binary(erlang:make_ref()),
  trans_i(Name, LockName, RandBin, Fun, LockTime, infinity);
trans(Name, LockName, Fun, LockTime, Timeout) when is_integer(Timeout) ->
  RandBin = erlang:term_to_binary(erlang:make_ref()),
  Now = os:perf_counter(millisecond),
  EndTime = Now + Timeout,
  trans_i(Name, LockName, RandBin, Fun, LockTime, EndTime).

trans_i(Name, LockKey, RandBin, Fun, LockTime, EndTime) ->
  Now = os:perf_counter(millisecond),
  Timeout = case EndTime of
              infinity -> false;
              _ -> Now >= EndTime
            end,
  if
    Timeout ->
      {error, timeout};
    true ->
      case redis_proxy:q(Name, ["SET", LockKey, RandBin, "PX", erlang:integer_to_list(LockTime), "NX"], ?REDIS_TIMEOUT) of
        {ok, <<"OK">>} ->
          Ret = case Fun of
                  {M, F, A} -> catch apply(M, F, A);
                  {F, A} when is_function(F) -> catch erlang:apply(F, A);
                  _ when is_function(Fun, 0) -> catch Fun()
                end,
          case redis_proxy:q(Name, ["GET", LockKey], ?REDIS_TIMEOUT) of
            {ok, RandBin} -> redis_proxy:q(Name, ["DEL", LockKey], ?REDIS_TIMEOUT);
            _ -> pass
          end,
          {ok, Ret};
        {ok, undefined} ->
          Left = EndTime - os:perf_counter(millisecond),
          if
            Left =< 1 ->
              {error, timeout};
            true ->
              Random = rand:uniform(?TRANS_SLEEP_TIME_MAX),
              Sleep = erlang:min(Random, Left - 1),
              timer:sleep(Sleep),
              trans_i(Name, LockKey, RandBin, Fun, LockTime, EndTime)
          end;
        Err ->
          % redis error
          Err
      end
  end.