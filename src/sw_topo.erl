-module(sw_topo).

-behaviour(gen_server).

-define(TIMETHRESH, 5*60).

-export([start_link/0,send_topo/2,get_topo/0,update_topo/0]).

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


send_topo(To, Msg) ->
  gen_server:cast(To, Msg).


get_topo() ->
  gen_server:call(?MODULE, get_topo).

update_topo() ->
  gen_server:cast(?MODULE, update_lldp).


%%=============================
%% gen_server callbacks
%%=============================

init([]) ->
  {ok, #state{}}.


handle_call(get_topo, _From, State) ->
  VNow = State#state.nowV,
  ENow = State#state.nowE,
  VLost = ordsets:subtract(State#state.maxV, VNow),
  ELost = ordsets:subtract(State#state.maxE, ENow),
  VN = ordsets:fold(fun format_ver_mac/2, ordsets:new(), VNow),
  EN = ordsets:fold(fun format_edg_mac/2, ordsets:new(), ENow),
  VL = ordsets:fold(fun format_ver_mac/2, ordsets:new(), VLost),
  EL = ordsets:fold(fun format_edg_mac/2, ordsets:new(), ELost),
  %% TODO format_mac
  {reply, [vertices, {VN, VL},
           edges, {EN, EL}],
   State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.


handle_cast(update_lldp, State) ->
  Now =
  calendar:datetime_to_gregorian_seconds(
    calendar:universal_time()),
  case State#state.epoch + ?TIMETHRESH > Now of
    true ->
      {noreply, State};
    false ->
      syn:publish(sw_topo, {update_lldp, self()}),
      {noreply, State#state{epoch = Now,
                            nowV = ordsets:new(),
                            nowE = ordsets:new()}}
  end;
handle_cast({local_topo, Vertices, Edges}, State) ->
  VNow = State#state.nowV,
  ENow = State#state.nowE,
  VMax = State#state.maxV,
  EMax = State#state.maxE,
  VNow1 = lists:foldl(fun ordsets:add_element/2, VNow, Vertices),
  ENow1 = lists:foldl(fun ordsets:add_element/2, ENow, Edges),
  VMax1 = lists:foldl(fun ordsets:add_element/2, VMax, Vertices),
  EMax1 = lists:foldl(fun ordsets:add_element/2, EMax, Edges),
  {noreply, State#state{nowV = VNow1,
                        nowE = ENow1,
                        maxV = VMax1,
                        maxE = EMax1}};
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

-spec format_edg_mac(Element, AccIn) ->
  AccOut when
    Element :: maps:map(),
    AccIn :: ordsets:ordset(),
    AccOut :: ordsets:ordset().
format_edg_mac(Element, AccIn) ->
  From = maps:get(from, Element),
  To = maps:get(to, Element),
  LocPortId = maybe_mac(maps:get(lldpLocPortId, Element)),
  RemPortId = maybe_mac(maps:get(lldpRemPortId, Element)),
  ordsets:add_element(
    Element#{from => octet_to_binary(From),
             to => octet_to_binary(To),
             lldpLocPortId => LocPortId,
             lldpRemPortId => RemPortId},
    AccIn).


-spec format_ver_mac(Element, AccIn) ->
  AccOut when
    Element :: maps:map(),
    AccIn :: ordsets:ordset(),
    AccOut :: ordsets:ordset().
format_ver_mac(Element, AccIn) ->
  ElementNew =
  case maps:get(lldpChassisIdSubtype, Element, unknown) of
    macAddress ->
      ChassisId = maps:get(lldpChassisId, Element),
      Element#{lldpChassisId => octet_to_binary(ChassisId)};
    _ ->
      Element
  end,
  ordsets:add_element(ElementNew, AccIn).


-spec maybe_mac([integer()]) -> binary().
maybe_mac(List) ->
  case length(List) of
    6 ->
      octet_to_binary(List);
    _ ->
      List
  end.


-spec octet_to_binary(Octs) -> binary() when
    Octs :: [integer()].
octet_to_binary(Octs) ->
  IoList =
  [io_lib:format("~2..0s", [integer_to_list(X, 16)])
   || X <- Octs],
  wf:to_binary(IoList).
