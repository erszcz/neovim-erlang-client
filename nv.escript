#!/usr/bin/env escript
%%!
%% -*- erlang -*-
%% vim: ft=erlang

-mode(compile).

main(_Args) ->
    print("alive 1~n"),
    {ok, F} = file:open("/tmp/dbg", write),
    print("alive 2~n"),
    %dbg:tracer(process, {fun dbg:dhandler/2, F}),
    %dbg:p(all, call),
    %dbg:tpl(?MODULE, x),
    %dbg:tpl(msgpack_rpc_fileio_server, x),
    %dbg:tpl(nvim_logger, x),
    print("alive 3~n"),

    code:add_path( ebin_dir(escript:script_name()) ),
    code:add_paths( deps_dirs(escript:script_name()) ),
    print("alive 4~n"),
    {ok, Pid} = msgpack_rpc_fileio_server:start_link(standard_io, undef, []),
    %MRef = erlang:monitor(process, Pid),
    print("alive 5~n"),
    receive
        {'DOWN', MRef, process, _Object, Info} ->
            print("alive 6~n"),
            print("io server down: ~p~n", [Info])
    end,
    print("alive 6~n").

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

ebin_dir(ScriptName) ->
    filename:join([base_dir(ScriptName), "ebin"]).
