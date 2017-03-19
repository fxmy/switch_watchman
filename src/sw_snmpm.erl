-module(sw_snmpm).

-include_lib("snmp/include/snmp_types.hrl").

-export([table/3]).


-spec table(User, Agent, Name) ->
  {ok, ProList} | {error, Why}
    when User :: term(),
         Agent :: term(),
         Name :: atom(),
         ProList :: proplists:proplist(),
         Why :: term().
table(User, Agent, Name) ->
  case snmpm:name_to_oid(Name) of
    {ok, [Oid]} ->
      table_recurse(User, Agent, Oid, Oid, []);
    {error, Why} ->
      {error, Why}
  end.


-spec table_recurse(User, Agent, OidRoot, OidPre, Acc) ->
  {ok, ProList} | {error, Why}
    when User:: term(),
         Agent :: term(),
         OidRoot :: list(),
         OidPre :: list(),
         Acc :: proplists:proplist(),
         ProList :: proplists:proplist(),
         Why :: term().
table_recurse(User, Agent, OidRoot, OidPre, Acc) ->
%  case lists:prefix(OidRoot, OidPre) of
%    false ->
%      {ok, Acc};
%    true ->
  %error_logger:info_msg("table_recurse: ~p, ~p, ~p", [OidRoot, OidPre, Acc]),
  case snmpm:sync_get_next(User, Agent, [OidPre]) of
    {error, Why} ->
      {error, Why};
    {ok, SnmpReply, _Remaining} ->
      case lists:prefix(OidRoot, extract_oid(SnmpReply)) of
        true ->
          %error_logger:info_msg("updating acc"),
          {AccNew, OidSelf} = update_acc(SnmpReply, Acc),
          table_recurse(User, Agent, OidRoot, OidSelf, AccNew);
        false ->
          {ok, Acc}
      end
  end.
%  end.


extract_oid({noError, _ErrIdx, [Varbind=#varbind{}]}) ->
  Varbind#varbind.oid;
extract_oid(_) ->
  [].

update_acc({noError, _ErrIdx, [Varbind=#varbind{}]}, Acc) ->
  Oid = Varbind#varbind.oid,
  VarType = Varbind#varbind.variabletype,
  Value = Varbind#varbind.value,
  {EntryIdx, EntryType} = mib_entry_type(Oid),
  Map = proplists:get_value(EntryIdx, Acc, #{}),
  Map1 = Map#{EntryType => {VarType, Value}},
  Acc1 = case proplists:is_defined(EntryIdx, Acc) of
           true ->
             lists:keyreplace(EntryIdx, 1, Acc, {EntryIdx, Map1});
           false ->
             [{EntryIdx, Map1} | Acc]
         end,
  %error_logger:info_msg("update_acc: ~p, ~p", [Acc1, Oid]),
  {Acc1, Oid}.


mib_entry_type(Oid) when is_list(Oid) ->
  mib_entry_type(Oid, []).

mib_entry_type(Oid, EntryIdx) ->
  %error_logger:info_msg("mib_entry_type: ~p, ~p", [Oid, EntryIdx]),
  case snmpm:oid_to_name(Oid) of
    {error, not_found} ->
      [EntryIdxUpper|OidUpper] = lists:reverse(Oid),
      mib_entry_type(lists:reverse(OidUpper), [EntryIdxUpper|EntryIdx]);
    {ok, EntryType} ->
      error_logger:info_msg("EntryIdx: ~p, EntryType: ~p~n", [EntryIdx, EntryType]),
      {EntryIdx, EntryType}
  end.
