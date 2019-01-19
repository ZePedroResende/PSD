-module(login_manager).
-export([start/0, create_account/2, close_account/2, login/3, logout/1, online/0, user_online/1]).

%interface functions

start() ->
    register(?MODULE, spawn(fun() -> loop(#{}) end)).

create_account(Username, Passwd) -> 
    ?MODULE ! {create_account, Username, Passwd, self()},
    receive 
        {?MODULE, Res} -> Res 
    end.

close_account(Username, Passwd) ->
    ?MODULE ! {close_account, Username, Passwd, self()},
    receive 
        {?MODULE, Res} -> Res 
    end.

login(Username, Passwd, Pid) ->
    ?MODULE ! {login, Username, Passwd, Pid, self()},
    receive 
        {?MODULE, Res} -> Res 
    end.

logout(Username) ->
    ?MODULE ! {logout, Username, self()},
    receive 
        {?MODULE, Res} -> Res 
    end.

user_online(Username) ->
    ?MODULE ! {user_online, Username, self()},
    receive 
        {?MODULE, Res} -> Res 
    end.

online() ->
    ?MODULE ! {online, self()},
    receive 
        {?MODULE, Res} -> Res 
    end.

%process 

loop(Map) ->
    receive
        {create_account, U, P, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, ok},
                    loop(maps:put(U, {P,false,0}, Map));
                _ ->
                    From ! {?MODULE, user_exists},
                    loop(Map)
            end;
        {close_account, U, P, From} ->
            case maps:find(U,Map) of
                {ok, {P,_,_}} ->
                    From ! {?MODULE, ok},
                    loop(maps:remove(U, Map));
                _ ->
                    From ! {?MODULE, error},
                    loop(Map)
            end;
        {login, U, P, Pid, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, error},
                    loop(Map);
                _ ->
                    From ! {?MODULE, ok},
                    loop(maps:put(U, {P,true,Pid}, Map))
            end;
        {logout, U, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, error},
                    loop(Map);
                {ok, {P,_,_}} ->
                    From ! {?MODULE, ok},
                    loop(maps:put(U, { P, false, 0}, Map))
            end;
        {user_online, U, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, error},
                    loop(Map);
                {ok, {_,_,Pid}} ->
                    From ! {?MODULE, Pid},
                    loop(Map)
            end;
        {online, From} ->
            Aux = fun(K,{_,BOOL},AccIn) when BOOL == true -> lists:append(K,AccIn) end,
            Users = maps:fold(Aux,[],Map),
            From ! {?MODULE, Users},
            loop(Map)
    end.