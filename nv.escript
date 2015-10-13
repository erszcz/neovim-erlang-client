#!/usr/bin/env escript
%%!
%% -*- erlang -*-
%% vim: ft=erlang

-mode(compile).

main(_Args) ->
    {ok, F} = file:open("/tmp/out", write),
    dbg:tracer(process, {fun dbg:dhandler/2, F}),
    dbg:p(all, call),
    dbg:tpl(?MODULE, x),
    code:add_paths( [base_dir(escript:script_name())] ),
    code:add_paths( deps_dirs(escript:script_name()) ),
    print("alive~n"),
    loop(standard_io, <<>>).

loop(Handle, Buffer) ->
    case file:read(Handle, 1) of
        {ok, Data} ->
            print("data: ~p~n", [Data]),
            {ok, NewBuffer} = process_data(cat(Buffer, Data)),
            loop(Handle, NewBuffer);
        eof ->
            print("eof");
        {error, R} ->
            print("error: ~p~n", [R])
    end.

process_data(Data) ->
    case msgpack:unpack_stream(Data) of
        {error, incomplete} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason};
        {Term, Remain} ->
            print("unpacked: ~p~n", [Term]),
            {ok, Remain}
    end.

cat(B1, B2) ->
    list_to_binary([B1, B2]).

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
