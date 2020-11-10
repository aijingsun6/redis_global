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
  {ok, Cfg} = application:get_env(redis_global, redis),
  Children = [
    {redis_proxy, {redis_proxy, start_link, [Cfg]}, permanent, 5000, worker, [redis_proxy]},
    {redis_global, {redis_global, start_link, []}, permanent, 5000, worker, [redis_global]}
  ],
  {ok, {{one_for_one, 1000, 3600}, Children}}.
