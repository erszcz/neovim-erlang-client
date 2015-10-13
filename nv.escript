#!/usr/bin/env escript
%%!
%% -*- erlang -*-
%% vim: ft=erlang

-mode(compile).

main(_Args) ->
    code:add_paths( [base_dir(escript:script_name())] ),
    code:add_paths( deps_dirs(escript:script_name()) ),
    {ok, Pid} = msgpack_rpc_fileio_server:start_link(standard_io, undef, []),
    MRef = erlang:monitor(process, Pid),
    print("alive~n"),
    receive
        {'DOWN', MRef, process, _Object, Info} ->
            print("io server down: ~p~n", [Info])
    end.

print(Text) ->
    print(Text, []).

print(Fmt, Args) ->
    %io:format(Fmt, Args).
    file:write_file("/tmp/out", io_lib:format(Fmt, Args), [append]).

base_dir(ScriptName) ->
    filename:absname(filename:dirname(ScriptName)).

deps_dirs(ScriptName) ->
    Base = base_dir(ScriptName),
    filelib:wildcard(filename:join([Base, "deps", "*", "ebin"])).
