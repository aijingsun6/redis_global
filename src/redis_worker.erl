-module(redis_worker).
-export([start_link/1]).

start_link(Args) ->
  Host = proplists:get_value(host, Args, "127.0.0.1"),
  Port = proplists:get_value(port, Args, 6379),
  Database = proplists:get_value(database, Args, 0),
  Password = proplists:get_value(password, Args, ""),
  eredis:start_link(Host, Port, Database, Password).