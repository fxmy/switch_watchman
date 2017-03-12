-module(login).
-compile(export_all).
%% -include_lib("kvs/include/feed.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
  wf:info(?MODULE, "login:main~n", []),
  #dtl{file="login",
       app=switch_watchman,
       bindings=[{body,body()},
                 {javascript, (?MODULE:(wf:config(n2o,mode,dev)))()}]}.

body() ->
 [ #span   { body= <<"芭芭拉小魔仙"/utf8>> },                #br{},
   #span   { body="Login: " },            #textbox{id=user,autofocus=true}, #br{},
   #span   { body="Join/Create Feed: " }, #textbox{id=pass},
   #button { id=loginButton, body="Login",postback=login,source=[user,pass]} ].

event(login) ->
    User = case wf:q(user) of <<>> -> "anonymous";
                              undefined -> "anonymous";
                              E -> wf:to_list(E) end,
    wf:user(User),
    wf:info(?MODULE,"User: ~p",[wf:user()]),
    wf:redirect("index.htm?room="++wf:to_list(wf:q(pass))),
    ok;

event(_) -> [].

prod() ->   [ #script{src="/static/express_deploy.min.js"} ].
dev()  -> [ [ #script{src=lists:concat(["/n2o/protocols/",X,".js"])} || X <- [bert,nitrogen] ],
            [ #script{src=lists:concat(["/n2o/",Y,".js"])}           || Y <- [bullet,xhr,n2o,ftp,utf8,validation] ] ].
