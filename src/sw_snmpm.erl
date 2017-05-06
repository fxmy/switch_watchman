-module(sw_snmpm).
-behavior(gen_server).

-include_lib("snmp/include/snmp_types.hrl").

-export([start_link/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state,{snmpm_user,snmpm_agent,
               scalars=[],tables=[]}).

-define(BULK_STEP, 80).


-spec start_link(User::term(),Agent::term(),proplists:prolist()) ->
  {ok, pid()} | {error, term()}.
start_link(User, Agent, ManagedObjects) ->
  gen_server:start_link(?MODULE, [User, Agent, ManagedObjects], []).


-spec init([any(),...]) ->
  {ok, #state{}} |
  {ok, #state{}, Timeout :: non_neg_integer()} |
  ignore |
  {stop, Reason :: any()}.
init([User, Agent, ManagedObjects]) ->
  Scalars =
  lists:flatten(
    proplists:get_all_values(scalar, ManagedObjects)),
  Tables =
  lists:flatten(
    proplists:get_all_values(table, ManagedObjects)),
  ok = syn:join(sw_topo, self()),
  {ok, #state{snmpm_user = User,
              snmpm_agent = Agent,
              scalars = Scalars,
              tables = Tables}}.


-spec handle_call(term(), term(), #state{}) ->
  {reply, ignore, #state{}}.
handle_call(_Req, _From, State) ->
  {reply, ignore, State}.


-spec handle_cast(term(), #state{}) ->
  {noreply, #state{}}.
handle_cast(_Req, State) ->
  {noreply, State}.


-spec handle_info({Info :: atom(), From :: pid()},
                  #state{}) ->
  {noreply, #state{}} |
  {noreply, #state{}, Timeout :: non_neg_integer()} |
  {stop, Reason :: any(), #state{}}.
handle_info({update_lldp, From},
            State=#state{snmpm_user=User,
                         snmpm_agent=Agent,
                         scalars=Scalars,
                         tables=Tables}) ->
  ScalarData =
  get_scalar_instances(User, Agent, Scalars),
  TableData =
  get_table_instances(User, Agent, Tables),
  {LocalVertex, RemVertices} =
  sw_lldp_util:parse_vertex(ScalarData, TableData),
  Edges = sw_lldp_util:parse_edge(LocalVertex, TableData),
  sw_topo:send_topo(From, {local_topo, [LocalVertex|RemVertices], Edges}),
  {noreply, State}.


-spec terminate(Reason :: any(), #state{}) -> terminated.
terminate(Reason, _State) ->
  error_logger:info_msg("Terminating sw_snmpm ~p with reason: ~p", [#state.snmpm_agent, Reason]),
  terminated.


-spec code_change(term()|{down,term()}, term(), term()) ->
  {ok, term()} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ============================================
%% Internal
%% ============================================

-spec get_scalar_instances(User :: term(), Agent :: term(), Scalars :: [atom()]) ->
  [tuple()].
get_scalar_instances(User, Agent, ScalarNames) ->
  Oids = [begin {ok,[Y]} = snmpm:name_to_oid(X),Y end || X <- ScalarNames],
  {ok, {noError, _ErrIdx, Varbinds}, _Remaining} =
  snmpm:sync_get_next(User, Agent, Oids),
  lists:zip(ScalarNames, Varbinds).


-spec get_table_instances(User, Agent, Tables) ->
  [tuple()] when User :: term(),
                 Agent :: term(),
                 Tables :: [atom()].
get_table_instances(User, Agent, Tables) ->
  TData = [begin
             case table(User, Agent, TableName) of
               {ok, Res} -> Res;
               {error, _} -> []
             end
           end
        || TableName <- Tables],
  lists:zip(Tables, TData).


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
  case snmpm:sync_get_bulk(User, Agent, 0, ?BULK_STEP, [OidPre]) of
    {error, Why} ->
      {error, Why};
    {ok, {noError, _ErrIdx, Varbinds}, _Remaining} ->
      wf:info(?MODULE, "BULK: ~p~n", [Varbinds]),
      filter_and_continue(User, Agent, Varbinds, OidRoot, OidPre, Acc)
  end.


-spec filter_and_continue(term(), term(), [#varbind{}],
                          list(), list(), proplists:proplist()) ->
  proplists:proplist().
filter_and_continue(User, Agent, [], OidRoot, OidPre, Acc) ->
  table_recurse(User, Agent, OidRoot, OidPre, Acc);
filter_and_continue(User, Agent, [H|T], OidRoot, _OidPre, Acc) ->
  case lists:prefix(OidRoot, H#varbind.oid) of
    false ->
      {ok, Acc};
    true ->
      {AccNew, OidSelf} = update_acc(H, Acc),
      filter_and_continue(User, Agent, T, OidRoot, OidSelf, AccNew)
  end.


-spec update_acc(#varbind{},
                 Acc :: proplists:proplist()) ->
  {Acc1 :: proplists:prolist(), Oid :: snmp:oid()}.
update_acc(Varbind=#varbind{}, Acc) ->
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
  {Acc1, Oid}.


-spec mib_entry_type(Oid :: snmp:oid()) ->
  {[integer()], atom()}.
mib_entry_type(Oid) when is_list(Oid) ->
  mib_entry_type(Oid, []).


-spec mib_entry_type(Oid :: snmp:oid(), [integer()]) ->
  {[integer()], atom()}.
mib_entry_type(Oid, EntryIdx) ->
  case snmpm:oid_to_name(Oid) of
    {error, not_found} ->
      [EntryIdxUpper|OidUpper] = lists:reverse(Oid),
      mib_entry_type(lists:reverse(OidUpper), [EntryIdxUpper|EntryIdx]);
    {ok, EntryType} ->
      {EntryIdx, EntryType}
  end.
