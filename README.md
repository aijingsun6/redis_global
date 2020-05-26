redis_global
=====
erlang global module impl by redis


### usage example
```
$ rebar3 run

1> redis_global_example:start_link(abc).
{ok,<0.291.0>}

2> P = redis_global:whereis_name(abc).
<0.291.0>

3> sys:get_state(P).
{state,abc}

4> redis_proxy:q(["keys","*"]).
{ok,[<<"redis_global_abc">>]}

5> ets:tab2list(redis_global_ets).
[{<<"redis_global_abc">>,<0.291.0>, #Ref<0.875029373.1213464577.7214>}]
```
