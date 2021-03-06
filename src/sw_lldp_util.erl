-module(sw_lldp_util).

-include_lib("snmp/include/snmp_types.hrl").
-include("include/LLDP-MIB.hrl").

-export([parse_vertex/2,parse_edge/2]).

-spec lldpPortIdSubtype(Varbind | integer()) ->
  atom() when Varbind :: snmp:varbind().
lldpPortIdSubtype(#varbind{value=Value}) ->
  port_type(Value);
lldpPortIdSubtype(Value) ->
  port_type(Value).

-spec port_type(integer()) -> atom().
port_type(Value) ->
  case Value of
    ?LldpPortIdSubtype_interfaceAlias ->
      interfaceAlias;
    ?LldpPortIdSubtype_portComponent ->
      portCompoent;
    ?LldpPortIdSubtype_macAddress ->
      macAddress;
    ?LldpPortIdSubtype_networkAddress ->
      networkAddress;
    ?LldpPortIdSubtype_interfaceName ->
      interfaceName;
    ?LldpPortIdSubtype_agentCircuitId ->
      agentCircuitId;
    ?LldpPortIdSubtype_local ->
      local;
    _ -> unknown
  end.

-spec lldpChassisIdSubtype(Varbind | integer()) ->
  atom() when Varbind :: snmp:varbind().
lldpChassisIdSubtype(#varbind{value=Value}) ->
  chassis_type(Value);
lldpChassisIdSubtype(Value) ->
  chassis_type(Value).

-spec chassis_type(integer()) -> atom().
chassis_type(Value) ->
  case Value of
    ?LldpChassisIdSubtype_chassisComponent ->
      chassisComponent;
    ?LldpChassisIdSubtype_interfaceAlias ->
      interfaceAlias;
    ?LldpChassisIdSubtype_portComponent ->
      portCompoent;
    ?LldpChassisIdSubtype_macAddress ->
      macAddress;
    ?LldpChassisIdSubtype_networkAddress ->
      networkAddress;
    ?LldpChassisIdSubtype_interfaceName ->
      interfaceName;
    ?LldpChassisIdSubtype_local ->
      local;
    _ ->
      unknown
  end.

-spec lldpSysName(Varbind | list()) ->
  binary() when Varbind :: snmp:varbind().
lldpSysName(#varbind{value=Value}) ->
  erlang:list_to_binary(Value);
lldpSysName(Name) ->
  erlang:list_to_binary(Name).

-spec lldpChassisId(Varbind | list()) ->
  list() when Varbind :: snmp:varbind().
lldpChassisId(#varbind{value=Value}) ->
  %% Res =
  %% lists:foldl(
  %%   fun(Int,Acc) ->
  %%       [erlang:integer_to_list(Int,16)|Acc] end,
  %%   [], Value),
  %% lists:reverse(Res).
  Value;
lldpChassisId(Id) ->
  Id.

%%%==============================
%%% vertices
%%%==============================

-spec parse_vertex(LocalScalar, Tables) ->
  {maps:map(), [maps:map()]} when
    LocalScalar :: proplists:proplist(),
    Tables :: proplists:prolist().
parse_vertex(LocalScalar, Tables) ->
  LocalVertex =
  lists:foldl(fun parse_local_vertex/2,
              #{}, LocalScalar),

  RemTable = proplists:get_value(lldpRemTable, Tables),
  RemVertices = parse_rem_vertex(RemTable),
  {LocalVertex, RemVertices}.

-spec parse_rem_vertex(RemTable | undefined) ->
  [maps:map()] when
    RemTable :: proplists:proplist().
parse_rem_vertex(undefined) ->
  [];
parse_rem_vertex(RemTable) ->
  lists:foldl(fun parse_rem_vertex/2,
              [], RemTable).

-spec parse_rem_vertex(RemTableEntry , AccIn) ->
  [maps:map()] when
    RemTableEntry :: {term(), maps:map()},
    AccIn :: [maps:map()].
parse_rem_vertex({_Idx, LldpRemMap}, AccIn)
  when is_map(LldpRemMap) ->
  {_, ChassisId} =
  maps:get(lldpRemChassisId,LldpRemMap,{undefined,undefined}),
  {_, ChassisIdSubtype} =
  maps:get(lldpRemChassisIdSubtype,LldpRemMap,{undefined,undefined}),
  {_, SysName} =
  maps:get(lldpRemSysName,LldpRemMap,{undefined,undefined}),
  [#{lldpChassisId => lldpChassisId(ChassisId),
     lldpChassisIdSubtype => lldpChassisIdSubtype(ChassisIdSubtype),
     lldpSysName => lldpSysName(SysName)}
   | AccIn].

-spec parse_local_vertex(LocalScalar, AccIn) ->
  maps:map() when
    LocalScalar :: {atom(), #varbind{}} |
    {atom(), atom()},
    AccIn :: maps:map().
parse_local_vertex({lldpLocSysName, Varbind=#varbind{}},
                   AccIn) ->
  NameBin = lldpSysName(Varbind),
  AccIn#{lldpSysName => NameBin};
parse_local_vertex({lldpLocChassisIdSubtype, Varbind=#varbind{}},
                   AccIn) ->
  TypeAtom = lldpChassisIdSubtype(Varbind),
  AccIn#{lldpChassisIdSubtype => TypeAtom};
parse_local_vertex({lldpLocChassisId, Varbind=#varbind{}},
                   AccIn) ->
  ChassisId = lldpChassisId(Varbind),
  AccIn#{lldpChassisId => ChassisId};
parse_local_vertex(_, AccIn) ->
  AccIn.

%%%==============================
%%% Edges
%%%==============================

-spec parse_edge(LocalVertex, TableData) ->
  [maps:map()]
    when LocalVertex :: maps:map(),
         TableData :: [tuple()].
parse_edge(LocalVertex, TableData) ->
  From = maps:get(lldpChassisId, LocalVertex),
  FromSubtype = maps:get(lldpChassisIdSubtype, LocalVertex),
  RemTable = proplists:get_value(lldpRemTable, TableData),
  LocPortTable = proplists:get_value(lldpLocPortTable, TableData),
  lists:foldl(rem_entry_2_edge(From, FromSubtype, LocPortTable),
              [], RemTable).

rem_entry_2_edge(From, FromSubtype, LocPortTable) ->
  fun({RemIdx, RemEntry}, AccIn) ->
      {_, To} = maps:get(lldpRemChassisId, RemEntry),
      {_, ToSubtype} = maps:get(lldpRemChassisIdSubtype, RemEntry),
      {_, RemPortId} = maps:get(lldpRemPortId, RemEntry),
      {_, RemPortIdSubtype} = maps:get(lldpRemPortIdSubtype, RemEntry),
      {_, RemPortDesc} = maps:get(lldpRemPortDesc, RemEntry),
      {_, LocPortId} =
      rem_tab_idx_2_loc_port(RemIdx, lldpLocPortId, LocPortTable),
      {_, LocPortIdSubtype} =
      rem_tab_idx_2_loc_port(RemIdx, lldpLocPortIdSubtype, LocPortTable),
      {_, LocPortDesc} =
      rem_tab_idx_2_loc_port(RemIdx, lldpLocPortDesc, LocPortTable),
      Res = #{from => From,
              fromSubtype => FromSubtype,
              to => To,
              toSubtype => lldpChassisIdSubtype(ToSubtype),
              lldpRemPortId => RemPortId,
              lldpRemPortIdSubtype => lldpPortIdSubtype(RemPortIdSubtype),
              lldpRemPortDesc => RemPortDesc,
              lldpLocPortId => LocPortId,
              lldpLocPortIdSubtype => lldpPortIdSubtype(LocPortIdSubtype),
              lldpLocPortDesc => LocPortDesc},
      [Res|AccIn]
  end.

-spec rem_tab_idx_2_loc_port(RemIdx, Key, LocPortTable) ->
  list()
    when RemIdx :: list(),
         Key :: atom(),
         LocPortTable :: proplists:proplist().
rem_tab_idx_2_loc_port([_LongInt, PortIdx, _NeighborIdx],
                       Key, LocPortTable) ->
  Map = proplists:get_value([PortIdx], LocPortTable, #{}),
  maps:get(Key, Map, "not_found").
