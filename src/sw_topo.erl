-module(sw_topo).

-behaviour(gen_server).

-define(TIMETHRESH, 5*60).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {epoch = 0,
                maxV = ordsets:new(),
                maxE = ordsets:new(),
                nowV = ordsets:new(),
                nowE = ordsets:new()}).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
  {ok, #state{}}.

handle_call(get_topo, _From, State) ->
  VNow = State#state.nowV,
  ENow = State#state.nowE,
  VLost = ordsets:subtract(State#state.maxV, VNow),
  ELost = ordsets:subtract(State#state.maxE, ENow),
  %% TODO format_mac
  {reply, 'TODO', State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.


handle_cast(update_lldp, State) ->
  syn:publish(sw_topo, {update_lldp, self()}),
  {noreply, State};
handle_cast({local_topo, Vertices, Edges}, State) ->
  %% TODO
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%===============================
%% Internal functions
%%===============================

-spec format_mac(Element, AccIn) ->
  AccOut when
    Element :: maps:map(),
    AccIn :: ordsets:ordset(),
    AccOut :: ordsets:ordset().
format_mac(Element, AccIn) ->
  %% TODO
  ordsets:new().
