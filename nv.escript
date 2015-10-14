#!/usr/bin/env escript
%%!
%% -*- erlang -*-
%% vim: ft=erlang

-mode(compile).

%-define(DEBUG, true).
-define(DEBUG, false).

-import(nvim_logger, [print/1, print/2]).

main(_Args) ->
    code:add_path( ebin_dir(escript:script_name()) ),
    code:add_paths( deps_dirs(escript:script_name()) ),
    ?DEBUG andalso enable_dbg(),
    Module = nvim_test,
    {ok, Pid} = msgpack_rpc_fileio_server:start_link(standard_io, Module, []),
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, _Object, Info} ->
            print("io server down: ~p~n", [Info])
    end.

base_dir(ScriptName) ->
    filename:absname(filename:dirname(ScriptName)).

deps_dirs(ScriptName) ->
    Base = base_dir(ScriptName),
    filelib:wildcard(filename:join([Base, "deps", "*", "ebin"])).

ebin_dir(ScriptName) ->
    filename:join([base_dir(ScriptName), "ebin"]).

enable_dbg() ->
    TraceFile = "/tmp/dbg",
    {ok, F} = file:open(TraceFile, write),
    dbg:tracer(process, {fun dbg:dhandler/2, F}),
    dbg:p(all, call),
    [ dbg:tpl(M, x)
      || M <- [?MODULE | list_modules(ebin_dir(escript:script_name()))] ].

list_modules(Dir) ->
    Wildcard = filename:join([Dir, "*.beam"]),
    Files = filelib:wildcard(Wildcard),
    [ list_to_atom(filename:basename(F, ".beam")) || F <- Files ].
