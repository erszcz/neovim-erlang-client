-module(msgpack_rpc_fileio_server).

-export([start_link/3]).

%% Internal
-export([init/3,
         parse_request/1]).

-include_lib("msgpack_rpc/include/msgpack_rpc.hrl").

-record(state, {iodev, buffer, module}).

start_link(IoDev, Module, Opts) ->
    Pid = spawn_link(?MODULE, init, [IoDev, Module, Opts]),
    {ok, Pid}.

init(IoDev, Module, _Opts) ->
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
            error_logger:error_msg("read error: ~p", [Reason]),
            error(Reason)
    end.

cat(B1, B2) ->
    list_to_binary([B1, B2]).

parse_request(#state{} = S) ->
    #state{buffer = Buffer, module = Module} = S,
    case msgpack:unpack_stream(Buffer) of
        {[?MP_TYPE_REQUEST, CallID, M, Argv] = R, Remain} ->
            nvim_logger:print("request ~p~n", [R]),
            spawn_request_handler(S#state.iodev, CallID, Module, M, Argv),
            parse_request(S#state{buffer = Remain});
        {[?MP_TYPE_NOTIFY, M, Argv] = N, Remain} ->
            nvim_logger:print("notify ~p~n", [N]),
            spawn_notify_handler(Module, M, Argv),
            parse_request(S#state{buffer = Remain});
        {error, incomplete} ->
            loop(S);
        {error, Reason} ->
            error_logger:error_msg("unpack error: ~p", [Reason]),
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
                    reply(IoDev, {reply, msgpack:pack(Prefix ++ [nil, Result])})
                catch
                    error:Reason ->
                        error_logger:error_msg("no such method: ~p / ~p", [Method, Reason]),
                        ReplyBin = msgpack:pack(Prefix ++ [error2binary(Reason), nil]),
                        reply(IoDev, {reply, ReplyBin});
                    Class:Throw ->
                        Error = lists:flatten(io_lib:format("~p:~p", [Class, Throw])),
                        error_logger:error_msg("(~p)~s", [self(), Error]),
                        case msgpack:pack(Prefix ++ [Error, nil]) of
                            {error, _Reason} ->
                                reply(IoDev, {reply, ["internal error", nil]});
                            Binary when is_binary(Binary) ->
                                reply(IoDev, {reply, Binary})
                        end
                end,
                erlang:demonitor(Ref)
        end,
    spawn(F).

reply(IoDev, {reply, Data}) ->
    file:write(IoDev, Data).

spawn_notify_handler(Module, M, Argv) ->
    spawn(fun() ->
                  Method = binary_to_existing_atom(M, latin1),
                  try
                      erlang:apply(Module, Method, Argv)
                  catch
                      Class:Throw ->
                          error_logger:error_msg("~p ~p:~p", [?LINE, Class, Throw])
                  end
          end).

-spec error2binary(atom())->binary().
error2binary(undef) -> <<"undef">>;
error2binary(function_clause) -> <<"function_clause">>.
