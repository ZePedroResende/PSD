-module(login_manager).
-export([start/0, create_account/3, close_account/2, login/2, logout/1, online/0, user_online/1]).

%interface functions

start() ->
    register(?MODULE, spawn(fun() -> loop(#{}) end)).

create_account(Username, Passwd, UserType) -> 
    ?MODULE ! {create_account, Username, Passwd, UserType, self()},
    receive 
        {?MODULE, Res, UT} -> {Res, UT} 
    end.

close_account(Username, Passwd) ->
    ?MODULE ! {close_account, Username, Passwd, self()},
    receive 
        {?MODULE, Res} -> Res 
    end.

login(Username, Passwd) ->
    ?MODULE ! {login, Username, Passwd, self()},
    receive 
        {?MODULE, Res, UT} -> {Res, UT}
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
        {create_account, U, P, UT, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, ok, UT},
                    loop(maps:put(U, {P,false,0,UT}, Map));
                _ ->
                    From ! {?MODULE, user_exists, UT},
                    loop(Map)
            end;
        {close_account, U, P, From} ->
            case maps:find(U,Map) of
                {ok, {P,_,_,_}} ->
                    From ! {?MODULE, ok},
                    loop(maps:remove(U, Map));
                _ ->
                    From ! {?MODULE, error},
                    loop(Map)
            end;
        {login, U, P, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, error, user_not_found},
                    loop(Map);
                {ok, {P,_,_,UT}} ->
                    From ! {?MODULE, ok, UT},
                    loop(maps:put(U, {P,true,From,UT}, Map))
            end;
        {logout, U, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, error},
                    loop(Map);
                {ok, {P,_,_,UT}} ->
                    From ! {?MODULE, ok},
                    loop(maps:put(U, { P, false, 0, UT}, Map))
            end;
        {user_online, U, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, error},
                    loop(Map);
                {ok, {_,_,Pid,_}} ->
                    From ! {?MODULE, Pid},
                    loop(Map)
            end;
        {online, From} ->
            Aux = fun(K,{_,BOOL},AccIn) when BOOL == true -> lists:append(K,AccIn) end,
            Users = maps:fold(Aux,[],Map),
            From ! {?MODULE, Users},
            loop(Map)
    end.