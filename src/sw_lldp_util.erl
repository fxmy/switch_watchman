-module(sw_lldp_util).

-include_lib("snmp/include/snmp_types.hrl").
-include("include/LLDP-MIB.hrl").
-compile([export_all]).

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
