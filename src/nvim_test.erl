-module(nvim_test).

-compile([export_all]).

-import(nvim_logger, [print/2]).

print_nvim_current_line() ->
    {ok, C} = msgpack_rpc_client:connect(tcp, localhost, 6666, []),
    R = msgpack_rpc_client:call(C, vim_get_current_line, []),
    print("~p:~p r = ~p~n", [?MODULE, ?LINE, R]).
