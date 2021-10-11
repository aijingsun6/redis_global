-module(redis_worker).
-export([start_link/1]).

start_link(Args) when is_list(Args) ->
  start_link(maps:from_list(Args));
start_link(Args) when is_map(Args) ->
  M = to_opts(Args),
  eredis:start_link(maps:to_list(M)).

to_opts(M) ->
  L = [host, port, database, username, password, reconnect_sleep, connect_timeout, socket_options, tls],
  M2 = maps:with(L, M),
  M3 = remove_undefined(username, M2),
  remove_undefined(password, M3).

remove_undefined(K, M) ->
  case maps:get(K, M, undefined) of
    undefined -> maps:remove(K, M);
    <<"undefined">> -> maps:remove(K, M);
    "undefined" -> maps:remove(K, M);
    _ -> M
  end.