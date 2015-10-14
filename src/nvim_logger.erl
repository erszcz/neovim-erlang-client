-module(nvim_logger).

-compile([export_all]).

-define(LOG, "/tmp/out").

print(Text) ->
    print(Text, []).

print(Fmt, Args) ->
    file:write_file("/tmp/out", io_lib:format(Fmt, Args), [append]).
