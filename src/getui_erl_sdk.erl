-module(getui_erl_sdk).
-export([init/0, start_getui/5, push/2]).
-on_load(init/0).

init() ->
    Cmd = priv_dir() ++ "/getui_nif_async",
    erlang:load_nif(Cmd, 0).

start_getui(Host, AppId, AppKey, MasterSecret, Workers) ->
    exit(nif_library_not_loaded).
push(Token, Data) ->
    exit(nif_library_not_loaded).

priv_dir() ->
    case code:priv_dir(acs) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                File when not is_list(File) -> "../priv";
                File -> filename:join([filename:dirname(File),"../priv"])
            end;
        F -> F
    end.
