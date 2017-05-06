-module(sw_topo).

-behaviour(gen_server).

-define(TIMETHRESH, 5*60).

-export([start_link/0,send_topo/2,get_topo/0,update_topo/0,web_notifier/0]).

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
                nowE = ordsets:new(),
                timer_ref}).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


send_topo(To, Msg) ->
  gen_server:cast(To, Msg).


get_topo() ->
  gen_server:call(?MODULE, get_topo).


update_topo() ->
  gen_server:cast(?MODULE, update_lldp).


web_notifier() ->
  update_topo(),
  timer:sleep(10000),
  wf:send(sw_topo_web, {client, display_topo}).


%%=============================
%% gen_server callbacks
%%=============================

init([]) ->
  erlang:process_flag(trap_exit, true),
  {ok, _} = timer:apply_after(5000, syn, publish, [sw_topo, {update_lldp, self()}]),
  {ok, Ref} = timer:apply_interval(1000*?TIMETHRESH, sw_topo, web_notifier, []),
  {ok, #state{timer_ref = Ref}}.


handle_call(get_topo, _From, State) ->
  VNow = State#state.nowV,
  ENow = State#state.nowE,
  VLost = ordsets:subtract(State#state.maxV, VNow),
  ELost = ordsets:subtract(State#state.maxE, ENow),
  VN = ordsets:fold(fun format_ver_id/2, ordsets:new(), VNow),
  EN = ordsets:fold(fun format_edg_id/2, ordsets:new(), ENow),
  VL = ordsets:fold(fun format_ver_id/2, ordsets:new(), VLost),
  EL = ordsets:fold(fun format_edg_id/2, ordsets:new(), ELost),
  {reply, [{vertices, {VN, VL}},
           {edges, {EN, EL}}],
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


terminate(_Reason, State) ->
  timer:cancel(State#state.timer_ref),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%===============================
%% Internal functions
%%===============================

-spec format_edg_id(Element, AccIn) ->
  AccOut when
    Element :: maps:map(),
    AccIn :: ordsets:ordset(),
    AccOut :: ordsets:ordset().
format_edg_id(Element, AccIn) ->
  From = maps:get(from, Element),
  FromSubtype = maps:get(fromSubtype, Element),
  To = maps:get(to, Element),
  ToSubtype = maps:get(toSubtype, Element),
  LocPortId = maps:get(lldpLocPortId, Element),
  LocPortIdType = maps:get(lldpLocPortIdSubtype, Element),
  RemPortId = maps:get(lldpRemPortId, Element),
  RemPortIdType = maps:get(lldpRemPortIdSubtype, Element),
  ordsets:add_element(
    Element#{from => format_printable(FromSubtype, From),
             to => format_printable(ToSubtype, To),
             lldpLocPortId => format_printable(LocPortIdType, LocPortId),
             lldpRemPortId => format_printable(RemPortIdType, RemPortId)},
    AccIn).


-spec format_ver_id(Element, AccIn) ->
  AccOut when
    Element :: maps:map(),
    AccIn :: ordsets:ordset(),
    AccOut :: ordsets:ordset().
format_ver_id(Element, AccIn) ->
  ChassisId = maps:get(lldpChassisId, Element),
  IdSubtype = maps:get(lldpChassisIdSubtype, Element, unknown),
  Id1 = format_printable(IdSubtype, ChassisId),
  ElementNew = Element#{lldpChassisId => Id1},
  ordsets:add_element(ElementNew, AccIn).


-spec format_printable(Type, Data) ->
  string() when Type :: atom, Data :: list().
format_printable(macAddress, Octs) ->
  [io_lib:format("~2..0s", [integer_to_list(X, 16)])
   || X <- Octs];
format_printable(networkAddress, Data) ->
  string:join([integer_to_list(X)||X<-Data], ".");
format_printable(_Other, Data) ->
  io_lib:format("~s", [Data]).
