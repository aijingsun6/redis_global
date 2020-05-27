-module(redis_global_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  Proxy = case erlang:whereis(redis_proxy) of
            Pid when is_pid(Pid) ->
              [];
            undefined ->
              Cfg = find_cfg(),
              [{redis_proxy, {redis_proxy, start_link, [Cfg]}, permanent, 5000, worker, [redis_proxy]}]
          end,
  Sub = [{redis_global_sub, {redis_global_sub, start_link, []}, permanent, 5000, supervisor, [redis_global_sub]}],
  RG = [{redis_global, {redis_global, start_link, []}, permanent, 5000, worker, [redis_global]}],
  Children = Proxy ++ Sub ++ RG,
  {ok, {{one_for_one, 1000, 3600}, Children}};

init([_Name]) ->
  Children = [{?MODULE, {?MODULE, proxy, []}, temporary, 5000, worker, [?MODULE]}],
  {ok, {{simple_one_for_one, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

find_cfg() ->
  case application:get_env(redis_global, redis) of
    {ok, V} -> V;
    _ -> #{}
  end.

