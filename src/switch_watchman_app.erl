%%%-------------------------------------------------------------------
%% @doc switch_watchman public API
%% @end
%%%-------------------------------------------------------------------

-module(switch_watchman_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    switch_watchman_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
