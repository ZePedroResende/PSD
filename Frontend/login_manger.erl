-module(login_manager).
-export([start/0, create_account/2, close_account/2, login/2, logout/1, online/0]).

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

login(Username, Passwd) ->
    ?MODULE ! {login, Username, Passwd, self()},
    receive 
        {?MODULE, Res} -> Res 
    end.

logout(Username) ->
    ?MODULE ! {logout, Username, self()},
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
                    loop(maps:put(U, {P,true}, Map));
                _ ->
                    From ! {?MODULE, user_exists},
                    loop(Map)
            end;
        {close_account, U, P, From} ->
            case maps:find(U,Map) of
                _ ->
                    From ! {?MODULE, error},
                    loop(Map);
                {ok, {P,_}} ->
                    From ! {?MODULE, ok},
                    loop(maps:remove(U, Map))
            end;
        {login, U, P, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, error},
                    loop(Map);
                _ ->
                    From ! {?MODULE, ok},
                    loop(maps:put(U, {P,true}, Map))
            end;
        {logout, U, From} ->
            case maps:find(U,Map) of
                error ->
                    From ! {?MODULE, error},
                    loop(Map);
                {P,_} ->
                    From ! {?MODULE, ok},
                    loop(maps:put(U, {P,false}, Map))
            end;
        {online, From} ->
            Aux = fun(K,{_,BOOL},AccIn) when BOOL == true -> lists:append(K,AccIn) end,
            Users = maps:fold(Aux,[],Map),
            From ! {?MODULE, Users},
            loop(Map)
    end.

online_status(K, {_, S}, AccIn) ->
    true.