redis_global
=====
erlang global module impl by redis



### usage example
```
$ source deploy_cfg.ini
$ rebar3 run
% start server in redis_global_1
(redis_global_1@localhost)1> redis_global_example:start_link(1).
{ok,<0.292.0>}
(redis_global_1@localhost)2> redis_global_example:start_link(2).
{ok,<0.294.0>}
(redis_global_1@localhost)3> redis_global_example:start_link(3).
{ok,<0.296.0>}
(redis_global_1@localhost)4> redis_global:i().
redis_global_3
redis_global_2
redis_global_4
redis_global_1
ok
% stop server in node redis_global_2
(redis_global_1@localhost)5> gen_server:stop(redis_global:whereis_name(4)).
ok

======================== node redis_global_2
(redis_global_2@localhost)1> redis_global:i().
redis_global_3
redis_global_2
redis_global_1
ok
(redis_global_2@localhost)2> redis_global_example:start_link(3).
{error,{already_started,<13969.296.0>}}
(redis_global_2@localhost)3> redis_global_example:start_link(4).
{ok,<0.300.0>}
(redis_global_2@localhost)4> redis_global:whereis_name(1).
<13969.292.0>
% server 4 terminate by node redis_global_1 operation
(redis_global_2@localhost)5> =ERROR REPORT==== 10-Nov-2020::18:42:45.834749 ===
4 terminate with normal

```
