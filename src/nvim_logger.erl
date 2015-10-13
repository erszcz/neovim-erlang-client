-module(nvim_logger).

-compile([export_all]).

print(Fmt, Args) ->
    %io:format(Fmt, Args).
    file:write_file("/tmp/out", io_lib:format(Fmt, Args), [append]).
