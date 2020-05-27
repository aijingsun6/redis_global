-module(redis_global_sub).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([
  start_link/0,
  init/1,
  start_child/3,
  proxy/3
]).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(M, F, A) ->
  supervisor:start_child(?SERVER, [M, F, A]).

proxy(M, F, A) ->
  erlang:apply(M, F, A).

init([]) ->
  Children = [{?MODULE, {?MODULE, proxy, []}, temporary, 5000, worker, [?MODULE]}],
  {ok, {{simple_one_for_one, 0, 1}, Children}}.

