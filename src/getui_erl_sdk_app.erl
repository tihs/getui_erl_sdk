-module(getui_erl_sdk_app).

-behaviour(application).

%% Application callbacks
-export([start/2, start/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    getui_erl_sdk_sup:start_link().

start() ->
    application:ensure_started(getui_erl_sdk).

stop(_State) ->
    ok.
