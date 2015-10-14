-module(msgpack_rpc_fileio_server).

-export([start_link/3]).

%% Internal
-export([init/3,
         parse_request/1]).

-include_lib("msgpack_rpc/include/msgpack_rpc.hrl").

-import(nvim_logger, [print/2]).

-record(state, {iodev, buffer, module}).

start_link(IoDev, Module, Opts) ->
    Pid = spawn_link(?MODULE, init, [IoDev, Module, Opts]),
    {ok, Pid}.

init(IoDev, Module, _Opts) ->
    print("~s:~p started, callback module: ~p~n", [?MODULE, ?LINE, Module]),
    loop(#state{iodev = IoDev,
                buffer = <<>>,
                module = Module}).

loop(#state{} = S) ->
    case file:read(S#state.iodev, 1) of
        {ok, Data} ->
            parse_request(S#state{buffer = cat(S#state.buffer, Data)});
        eof ->
            ok;
        {error, Reason} ->
            print("read error: ~p~n", [Reason]),
            error(Reason)
    end.

cat(B1, B2) ->
    list_to_binary([B1, B2]).

parse_request(#state{} = S) ->
    #state{buffer = Buffer, module = Module} = S,
    case msgpack:unpack_stream(Buffer) of
        {[?MP_TYPE_REQUEST, CallID, M, Argv] = R, Remain} ->
            print("request: ~p~n", [R]),
            spawn_request_handler(S#state.iodev, CallID, Module, M, Argv),
            loop(S#state{buffer = Remain});
        {[?MP_TYPE_NOTIFY, M, Argv] = N, Remain} ->
            print("notify: ~p~n", [N]),
            spawn_notify_handler(Module, M, Argv),
            loop(S#state{buffer = Remain});
        {{Term}, Remain} ->
            print("not an RPC: ~p~n", [Term]),
            loop(S#state{buffer = Remain});
        {error, incomplete} ->
            loop(S);
        {error, Reason} ->
            print("unpack error: ~p~n", [Reason]),
            error(Reason)
    end.

spawn_request_handler(IoDev, CallID, Module, M, Argv) ->
    Pid = self(),
    F = fun() ->
                Ref = erlang:monitor(process, Pid),
                Method = binary_to_existing_atom(M, latin1),
                Prefix = [?MP_TYPE_RESPONSE, CallID],
                try
                    Result = erlang:apply(Module,Method,Argv),
                    reply(IoDev, {reply, Prefix ++ [nil, Result]})
                catch
                    Error:Reason ->
                        print("~p:~p ~p:~p~n", [?MODULE, ?LINE, Error, Reason]),
                        reply(IoDev, {reply, Prefix ++ [error2binary(Reason), nil]})
                end,
                erlang:demonitor(Ref)
        end,
    spawn(F).

reply(IoDev, {reply, Reply}) ->
    print("reply: ~p~n", [Reply]),
    case msgpack:pack(Reply) of
        {error, Reason} ->
            print("pack error: ~p~n", [Reason]),
            error(pack_error, [Reply]);
        B when is_binary(B) ->
            file:write(IoDev, B)
    end.

spawn_notify_handler(Module, M, Argv) ->
    spawn(fun() ->
                  Method = binary_to_existing_atom(M, latin1),
                  try
                      erlang:apply(Module, Method, Argv)
                  catch
                      Error:Reason ->
                          print("~p:~p ~p:~p~n", [?MODULE, ?LINE, Error, Reason])
                  end
          end).

-spec error2binary(atom()) -> binary().
error2binary(undef) -> <<"undef">>;
error2binary(function_clause) -> <<"function_clause">>;
error2binary(_) -> <<"internal error">>.
