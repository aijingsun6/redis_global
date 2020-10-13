-module(redis_worker).
-export([start_link/1]).

start_link(Args) when is_list(Args) ->
  Host = proplists:get_value(host, Args, "127.0.0.1"),
  Port = proplists:get_value(port, Args, 6379),
  Database = proplists:get_value(database, Args, 0),
  Password = proplists:get_value(password, Args, ""),
  eredis:start_link(Host, Port, Database, Password);
start_link(Args) when is_map(Args) ->
  Host = maps:get(host, Args, "127.0.0.1"),
  Port = maps:get(port, Args, 6379),
  Database = maps:get(database, Args, 0),
  Password = maps:get(password, Args, ""),
  eredis:start_link(Host, Port, Database, Password).