-module(redis_global).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").
-export([
  start_link/0,
  start_link/1,
  i/0
]).

-export([
  unregister_name/1,
  whereis_name/1,
  register_name/2
]).

-export([
  to_redis_name/1,
  unregister_name/2,
  is_process_alive/1,
  svr_name/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).
-define(SERVER, ?MODULE).
%% {Name, Pid, Ref}
-define(REDIS_GLOBAL_ETS, redis_global_ets).
-define(CALL_TIMEOUT, 5000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-record(state, {
  monitor_map = #{}
}).

start_link() ->
  start_link([]).

start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], Options).

svr_name(Name) ->
  {via, redis_global, Name}.

whereis_name(Name) ->
  RedisName = to_redis_name(Name),
  Pid =
    case ets:lookup(?REDIS_GLOBAL_ETS, RedisName) of
      [{_, P, _}] -> P;
      [] -> whereis_name_by_redis_name(RedisName)
    end,
  case is_pid(Pid) of
    true ->
      case ?MODULE:is_process_alive(Pid) of
        true -> Pid;
        false -> undefined
      end;
    false ->
      undefined
  end.

register_name(Name, Pid) ->
  case whereis_name(Name) of
    undefined ->
      RedisName = to_redis_name(Name),
      gen_server:call(?SERVER, {register_name, RedisName, Pid}, ?CALL_TIMEOUT);
    Pid -> yes;
    _ -> no

  end.

unregister_name(Name) ->
  case whereis_name(Name) of
    undefined -> ok;
    Pid when is_pid(Pid) ->
      RedisName = to_redis_name(Name),
      case erlang:node(Pid) of
        N when N =:= node() -> gen_server:call(?SERVER, {unregister_name, RedisName, Pid}, ?CALL_TIMEOUT);
        N2 -> gen_server:call({?SERVER, N2}, {unregister_name, RedisName, Pid}, ?CALL_TIMEOUT)
      end
  end.

unregister_name(Name, Pid) ->
  gen_server:call(?SERVER, {unregister_name, Name, Pid}, ?CALL_TIMEOUT).

is_process_alive(Pid) when is_pid(Pid) ->
  Node = erlang:node(Pid),
  case Node =:= node() of
    true ->
      erlang:is_process_alive(Pid);
    false ->
      case rpc:call(Node, erlang, is_process_alive, [Pid], 5000) of
        Ret when is_boolean(Ret) -> Ret;
        {badrpc, _} -> false
      end
  end;
is_process_alive(_) ->
  false.

i() ->
  Reg = to_redis_name(<<"*">>),
  F = fun(L) -> lists:foreach(fun(X) -> io:format("~ts~n", [X]) end, L) end,
  redis_proxy:foreach_redis(Reg, F, 1024, ?CALL_TIMEOUT).

to_redis_name(Name) when is_list(Name); is_binary(Name) ->
  S = lists:flatten(io_lib:format("~p_~ts", [?MODULE, Name])),
  unicode:characters_to_binary(S);
to_redis_name(Name) ->
  S = lists:flatten(io_lib:format("~p_~p", [?MODULE, Name])),
  unicode:characters_to_binary(S).

init([]) ->
  process_flag(trap_exit, true),
  ets:new(?REDIS_GLOBAL_ETS, [named_table, set, {read_concurrency, true}]),
  {ok, #state{}}.

handle_call({unregister_name, RedisName, Pid}, _From, #state{monitor_map = M} = S) when is_binary(RedisName) ->
  ?LOG_DEBUG("unregister_name,~ts,~p", [RedisName, Pid]),
  M2 = case ets:lookup(?REDIS_GLOBAL_ETS, RedisName) of
         [] -> M;
         [{_, Pid, Ref}] ->
           erlang:demonitor(Ref, [flush]),
           ets:delete(?REDIS_GLOBAL_ETS, RedisName),
           maps:remove(Ref, M);
         [_] -> M
       end,
  proc_lib:spawn(fun() -> unregister_name_i(RedisName, Pid) end),
  {reply, ok, S#state{monitor_map = M2}};
handle_call({register_name, RedisName, Pid}, _From, #state{monitor_map = M} = S) when is_binary(RedisName) ->
  ?LOG_DEBUG("register_name,~ts,~p", [RedisName, Pid]),
  R = register_name_i(RedisName, Pid),
  M2 = case R of
         yes ->
           Ref = erlang:monitor(process, Pid),
           ets:insert(?REDIS_GLOBAL_ETS, {RedisName, Pid, Ref}),
           M#{Ref => RedisName};
         no ->
           M
       end,
  {reply, R, S#state{monitor_map = M2}};
handle_call(Request, _From, S) ->
  ?LOG_WARNING("unhandled request:~p", [Request]),
  {reply, ok, S}.

handle_cast(Request, S) ->
  ?LOG_WARNING("unhandled request:~p", [Request]),
  {noreply, S}.

handle_info({'DOWN', Ref, _Type, _Object, _Info}, #state{monitor_map = M} = S) ->
  erlang:demonitor(Ref, [flush]),
  case maps:get(Ref, M, undefined) of
    undefined ->
      {noreply, S};
    Name ->
      ?LOG_INFO("~ts down", [Name]),
      case ets:lookup(?REDIS_GLOBAL_ETS, Name) of
        [{_, Pid, Ref}] ->
          proc_lib:spawn(fun() -> unregister_name_i(Name, Pid) end),
          ets:delete(?REDIS_GLOBAL_ETS, Name);
        _ ->
          pass
      end,
      M2 = maps:remove(Ref, M),
      {noreply, S#state{monitor_map = M2}}
  end;
handle_info(Request, S) ->
  ?LOG_WARNING("unhandled request:~p", [Request]),
  {noreply, S}.

terminate(Reason, _S) ->
  ?LOG_WARNING("~p terminate,reason ~p", [?SERVER, Reason]),
  Total = ets:info(?REDIS_GLOBAL_ETS, size),
  Self = self(),
  ets:foldl(fun({Name, Pid, Ref}, Acc) ->
    erlang:demonitor(Ref, [flush]),
    proc_lib:spawn(fun() -> shutdown(Self, Name, Pid, Reason) end),
    Acc
            end, [], ?REDIS_GLOBAL_ETS),
  recv_shutdown_sig(0, Total),
  ?LOG_WARNING("~p terminate ~p procs", [?SERVER, Total]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ======
%% Pid | undefined
whereis_name_by_redis_name(Name) ->
  case redis_proxy:q(["GET", Name]) of
    {ok, Bin} when is_binary(Bin) ->
      case erlang:binary_to_term(Bin) of
        Pid when is_pid(Pid) ->
          Pid;
        _ ->
          undefined
      end;
    {ok, undefined} ->
      undefined;
    {error, _} ->
      undefined
  end.

unregister_name_i(Name, Pid) when is_binary(Name) ->
  LockKey = erlang:iolist_to_binary([Name, "_lock"]),
  F = fun() ->
    case whereis_name_by_redis_name(Name) of
      Pid -> redis_proxy:q(["DEL", Name]);
      _ -> pass
    end end,
  {ok, Result} = redis_proxy:trans(LockKey, F),
  Result.

%% yes | no
register_name_i(Name, Pid) when is_binary(Name) ->
  LockKey = erlang:iolist_to_binary([Name, "_lock"]),
  F = fun() ->
    case whereis_name_by_redis_name(Name) of
      undefined -> set_name(Name, Pid);
      P when P =:= Pid ->
        yes;
      P ->
        case ?MODULE:is_process_alive(P) of
          true -> no;
          false -> set_name(Name, Pid)
        end
    end
      end,

  {ok, Result} = redis_proxy:trans(LockKey, F),
  Result.

%% yes | no
set_name(Name, Pid) ->
  case redis_proxy:q(["SET", Name, erlang:term_to_binary(Pid)]) of
    {ok, _} ->
      ?LOG_DEBUG("set_name_success ~ts -> ~p", [Name, Pid]),
      yes;
    {error, Msg} ->
      ?LOG_WARNING("set_name_fail ~ts -> ~p, ~p", [Name, Pid, Msg]),
      no
  end.

shutdown(Self, Name, Pid, Reason) ->
  ?LOG_INFO("shuwdown ~ts,~p, reason:~p", [Name, Pid, Reason]),
  gen:stop(Pid, Reason, infinity),
  case whereis_name_by_redis_name(Name) of
    Pid -> redis_proxy:q(["DEL", Name]), ok;
    _ -> pass
  end,
  Self ! ok.

recv_shutdown_sig(Acc, Total) when Acc >= Total -> ok;
recv_shutdown_sig(Acc, Total) ->
  receive
    ok ->
      recv_shutdown_sig(Acc + 1, Total)
  after 2000 ->
    recv_shutdown_sig(Acc + 1, Total)
  end.