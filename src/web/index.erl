-module(index).
-compile(export_all).
%% -include_lib("kvs/include/feed.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
  wf:info(?MODULE, "~p~n", [wf:path(?REQ)]),
  wf:redirect("topo"),
  #dtl{%file="login",
       app=switch_watchman,
       bindings=[{body,[]},
                 {javascript, (?MODULE:(wf:config(n2o,mode,dev)))()}]}.

prod() ->   [ #script{src="/static/express_deploy.min.js"} ].
dev()  -> [ [ #script{src=lists:concat(["/n2o/protocols/",X,".js"])} || X <- [bert,nitrogen] ],
            [ #script{src=lists:concat(["/n2o/",Y,".js"])}           || Y <- [bullet,xhr,n2o,ftp,utf8,validation] ] ].
