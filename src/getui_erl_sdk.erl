-module(getui_erl_sdk).
-export([init/0, start_getui/5, push_trans/2, push_notifi/3]).
-on_load(init/0).

init() ->
    Cmd = priv_dir() ++ "/getui_nif_async",
    erlang:load_nif(Cmd, 0).

start_getui(Host, AppId, AppKey, MasterSecret, Workers) ->
    exit(nif_library_not_loaded).
push_trans(Token, Data) ->
    exit(nif_library_not_loaded).
push_notifi(Token, Title, Content) ->
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
