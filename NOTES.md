# Manual testing of `msgpack:unpack_stream/1`

Shell #1:

```
tail -f /tmp/out
```

Shell #2:

```
> P = erlang:open_port({spawn, "./nv.escript"}, []).
> erlang:port_command(P, msgpack:pack(<<"c">>)).
```
