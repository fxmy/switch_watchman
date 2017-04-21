-module(topo).
-compile(export_all).
%% -include_lib("kvs/include/feed.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
  % wf:info(?MODULE, "~p, ~p~n", [wf:session_id(), self()]),
  #dtl{file="topo",
       app=switch_watchman,
       bindings=[{body,[<<"和出错"/utf8>>]},
                 {javascript, (?MODULE:(wf:config(n2o,mode,dev)))()}]}.

prod() ->   [ #script{src="/static/express_deploy.min.js"} ].
dev()  -> [ [ #script{src=lists:concat(["/n2o/protocols/",X,".js"])} || X <- [bert,nitrogen] ],
            [ #script{src=lists:concat(["/n2o/",Y,".js"])}           || Y <- [bullet,xhr,n2o,ftp,utf8,validation] ],
            [ #script{src="/assets/vis.min.js"}]
          ].


event(init) ->
  Res = wf:async("topo_countdown", fun topo:loop/1),
  n2o_async:send("topo_countdown", {5000,self()}),
  wf:info(?MODULE, "init ~p, ~p~n", [self(), Res]);
event(terminate) ->
  wf:stop("topo_countdown"),
  wf:info(?MODULE, "terminate ~p~n", [self()]);
event(E) ->
  wf:info(?MODULE, "event ~p, ~p~n", [E, self()]).


loop({T,Pid}) when is_integer(T), is_pid(Pid) ->
  wf:info(?MODULE, "loop target: ~p~n", [Pid]),
  % adhere N2O client prottocol
  % erlang:send_after(T, Pid, {client, update_lldp}),
  timer:apply_after(5000, wf, send, []).
