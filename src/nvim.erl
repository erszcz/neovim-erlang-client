-module(nvim).

-export([api_info/0, api_info/1,
         help/1]).

api_info() ->
    PackedAPIInfo = list_to_binary(os:cmd("nvim --api-info")),
    {ok, Info} = msgpack:unpack(PackedAPIInfo),
    decode_msgpack_format(Info).

api_info(Function) ->
    {<<"functions">>, Functions} = lists:keyfind(<<"functions">>, 1, api_info()),
    case [ F || F <- Functions,
                proplists:get_value(<<"name">>, F) == Function ]
    of
        [] -> not_found;
        [Info] -> Info
    end.

help(Function) ->
    api_info(list_to_binary(atom_to_list(Function))).

decode_msgpack_format(V) when
      is_binary(V);
      is_boolean(V);
      is_integer(V);
      is_float(V) ->
    V;
decode_msgpack_format({K, V}) ->
    {K, decode_msgpack_format(V)};
decode_msgpack_format(L) when is_list(L) ->
    [ decode_msgpack_format(Elem) || Elem <- L ];
decode_msgpack_format({L}) when is_list(L) ->
    [ decode_msgpack_format(Elem) || Elem <- L ].
