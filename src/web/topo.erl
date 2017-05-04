-module(topo).
-export([main/0,event/1]).
-export([prod/0,dev/0]).
%% -include_lib("kvs/include/feed.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
  % wf:info(?MODULE, "~p, ~p~n", [wf:session_id(), self()]),
  #dtl{file="topo",
       app=switch_watchman,
       bindings=[{body, body()},
                 {javascript, (?MODULE:(wf:config(n2o,mode,dev)))()}]}.

prod() ->   [ #script{src="/static/express_deploy.min.js"} ].
dev()  -> [ [ #script{src=lists:concat(["/n2o/protocols/",X,".js"])} || X <- [bert,nitrogen] ],
            [ #script{src=lists:concat(["/n2o/",Y,".js"])}           || Y <- [bullet,xhr,n2o,ftp,utf8,validation] ],
            [ #script{src="/assets/vis.min.js"}]
          ].


body() ->
  [#panel{id=topo_net}].


event(init) ->
  wf:reg(sw_topo_web),
  wf:wire([draw_topo()]);
event(terminate) ->
  wf:unreg(sw_topo_web);
event(#client{data=display_topo}) ->
  wf:wire(<<"console.log(Date().toLocaleString());"/utf8>>),
  wf:wire([vis_clear(),draw_topo()]);
event(E) ->
  wf:info(?MODULE, "event ~p, ~p~n", [E, self()]).


draw_topo() ->
  PrLs = sw_topo:get_topo(),
  {VOnline, VOffline} = proplists:get_value(vertices,PrLs),
  {EOnline, EOffline} = proplists:get_value(edges,PrLs),
  WV1 = [begin
           I = maps:get(lldpChassisId, X, "wat?!"),
           N = maps:get(lldpSysName, X, "wat?!"),
           vis_add_node(I,N,color_online(),online)
         end || X <- VOnline],
  WV2 = [begin
           I = maps:get(lldpChassisId, X, "wat?!"),
           N = maps:get(lldpSysName, X, "wat?!"),
           vis_add_node(I,N,color_offline(),offline)
         end || X <- VOffline],
  WE1 = [begin
           F = maps:get(from, X, "wat?!"),
           T = maps:get(to, X, "wat?!"),
           PortF = maps:get(lldpLocPortId, X, "wat?!"),
           PortT = maps:get(lldpRemPortId, X, "wat?!"),
           DescF = maps:get(lldpLocPortDesc, X, "wat?!"),
           DescT = maps:get(lldpRemPortDesc, X, "wat?!"),
           vis_add_edge(F, T, [PortF," -> ",PortT],
                        [DescF, " -> ",DescT],
                        color_online(), online)
         end|| X <- EOnline],
  WE2 = [begin
           F = maps:get(from, X, "wat?!"),
           T = maps:get(to, X, "wat?!"),
           PortF = maps:get(lldpLocPortId, X, "wat?!"),
           PortT = maps:get(lldpRemPortId, X, "wat?!"),
           DescF = maps:get(lldpLocPortDesc, X, "wat?!"),
           DescT = maps:get(lldpRemPortDesc, X, "wat?!"),
           vis_add_edge(F, T, [PortF," -> ",PortT],
                        [DescF, " -> ",DescT],
                        color_offline(), offline)
         end|| X <- EOffline],

  [vis_init_network(), WV1, WV2, WE1, WE2, vis_network(), "vis_topo.stabilize(1000);"].


%%% visjs
vis_container() ->
  ["ct = document.getElementById('topo_net');"].

vis_options() ->
  ["opt = {nodes:{shape:'box',borderWidth:2,shadow:true},
   edges:{width:1,shadow:true,smooth:{type:'continuous',roundness:0.9},length:300,arrows:{middle:{scaleFactor:0.5}}},
   interaction:{hideEdgesOnDrag: true},layout:{hierarchical:{direction:'UD',levelSeparation:300}},
   physics:{enabled:false,hierarchicalRepulsion:{nodeDistance:200,centralGravity:0}}};"].

%% layout:{hierarchical:{direction:'UD'}},
%% physics:{enabled:false,forceAtlas2Based:{centralGravity:0.01,gravitationalConstant:50}}};"].
%% physics:{enabled:true,barnesHut:{centralGravity:0.005,gravitationalConstant:100}}};"].
%%
%%opt = {
%%      nodes:{shape:'box',
%%             borderWidth:2,
%%             shadow:true},
%%      edges:{width:1,
%%             shadow:true,
%%             smooth:{type:'continuous',
%%                     roundness:0.9},
%%             length:300,
%%             arrows:{middle:{scaleFactor:0.5}}},
%%      interaction:{hideEdgesOnDrag: true},
%%      layout:{hierarchical:{direction:'UD',
%%                            levelSeparation:300}},
%%      physics:{enabled:true,
%%               hierarchicalRepulsion:{nodeDistance:200,
%%                                      centralGravity:0}}};


vis_nodes() ->
  ["var nds = new vis.DataSet([]);"].

vis_edges() ->
  ["var egs = new vis.DataSet([]);"].

vis_data() ->
  ["vis_dt = {nodes: nds, edges: egs};"].

vis_network() ->
  ["vis_topo = new vis.Network(ct, vis_dt, opt);"].

vis_init_network() ->
  N = vis_nodes(), E = vis_edges(), C = vis_container(), D = vis_data(), O = vis_options(),
  [N, E, C, D, O].

vis_clear() ->
  ["vis_topo.destroy();"].

vis_add_node(Id, Name, Color, online) ->
  ["vis_dt.nodes.add({id:'",Id,"',label:'",Name,"\\n",Id,"',color:'",Color,"'});"];
vis_add_node(Id, Name, Color, offline) ->
  ["vis_dt.nodes.add({id:'",Id,"',label:'",Name,"\\n",Id,"',color:'",Color,"',shapeProperties:{borderDashes:[10,5]}});"].

vis_add_edge(From, To, Label, Title, Color, online) ->
  ["vis_dt.edges.add({from:'",From,"',to:'",To,"',color:'",Color,"',font: {align: 'middle'},title:'",Label, "<br>", Title,"'});"];
vis_add_edge(From, To, Label, Title, Color, offline) ->
  ["vis_dt.edges.add({from:'",From,"',to:'",To,"',color:'",Color,"',font: {align: 'middle'},title:'",Label, "<br>", Title,"',dashes:true});"].


color_online() -> "#00DA7D".
color_offline() -> "#FF692D".
