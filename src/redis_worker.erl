-module(redis_worker).
-export([start_link/1]).

start_link(Args) when is_list(Args) ->
  eredis:start_link(to_opts(Args, []));
start_link(Args) when is_map(Args) ->
  Opts = to_opts(maps:to_list(Args), []),
  eredis:start_link(Opts).

to_opts([], Acc) ->
  Acc;

to_opts([{K, V} | L], Acc) when K =:= username; K =:= password ->
  if
    is_binary(V) andalso V =/= <<>> -> to_opts(L, [{K, V} | Acc]);
    is_list(V) andalso V =/= [] -> to_opts(L, [{K, V} | Acc]);
    true -> to_opts(L, Acc)
  end;

to_opts([{K, V} | L], Acc) ->
  KS = [host, port, database, reconnect_sleep, connect_timeout, socket_options, tls],
  IsKey = lists:member(K, KS),
  if
    IsKey ->
      to_opts(L, [{K, V} | Acc]);
    true ->
      to_opts(L, Acc)
  end.


