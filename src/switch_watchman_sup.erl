%%%-------------------------------------------------------------------
%% @doc switch_watchman top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(switch_watchman_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  syn:init(),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 0, 1},
           [spec(),
            sw_topo(),
            sw_snmpm_sup()]}}.

%%====================================================================
%% Internal functions
%%====================================================================
sw_topo() ->
  {sw_topo, {sw_topo, start_link, []}, permanent, 5000, worker, [sw_topo]}.
sw_snmpm_sup() ->
  {sw_snmpm_sup, {sw_snmpm_sup, start_link, []}, permanent, 5000, supervisor, [sw_snmpm_sup]}.
spec()   -> ranch:child_spec(http, 100, ranch_tcp, port(), cowboy_protocol, env()).
env()    -> [ { env, [ { dispatch, points() } ] } ].
static() ->   { dir, "priv/assets", mime() }.
n2o()    ->   { dir, "deps/n2o/priv",           mime() }.
mime()   -> [ { mimetypes, cow_mimetypes, all   } ].
port()   -> [ { port, wf:config(n2o,port,8000)  } ].
points() -> cowboy_router:compile([{'_', [

    {"/assets/[...]",       n2o_static,  static()},
    {"/n2o/[...]",          n2o_static,  n2o()},
    {"/multipart/[...]",  n2o_multipart, []},
    {"/rest/:resource",     rest_cowboy, []},
    {"/rest/:resource/:id", rest_cowboy, []},
    {"/ws/[...]",           n2o_stream,  []},
    {'_',                   n2o_cowboy,  []} ]}]).
