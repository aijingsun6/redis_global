-module(redis_global_example).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  start_link/1,
  start_in_sup/0,
  start_in_sup/1
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


-record(state, {name}).

start_link() ->
  start_link(?MODULE).

start_link(Name) ->
  SName = redis_global:svr_name(Name),
  gen_server:start_link(SName, ?MODULE, [Name], []).

start_in_sup() ->
  start_in_sup(?MODULE).

start_in_sup(Name) ->
  redis_global_sub:start_child(?MODULE, start_link, [Name]).

init([Name]) ->
  process_flag(trap_exit, true),
  {ok, #state{name = Name}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, #state{name = Name}) ->
  ?LOG_ERROR("~p terminate with ~p", [Name, Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================