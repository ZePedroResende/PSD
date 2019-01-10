-module(chatv3).
-export([server/1]).

server(Port) ->
  Room = spawn(fun()-> room([],#{}) end),
  Room ! {room_init, "default", Room},
  login_manager:start(),
  {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
  acceptor(LSock, Room).

acceptor(LSock, Room) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock, Room) end),
  handleauth(Sock, Room).

handleauth(Sock, Room) ->
  receive
    {tcp, _, Data} ->
      Msg = protocol:decode_msg(Data, 'Message'),
      Cli = maps:get(user, Msg),
      User = maps:get(username, Cli),
      Pass = maps:get(password, Cli),
      case maps:get(type, Msg) of
        "REGISTER" ->
          registarhandler(Sock, Room, User, Pass);
        "LOGIN" ->
          loginhandler(Sock, Room, User, Pass)
      end;
    {tcp_closed, _} ->
      false;
    {tcp_error, _, _} ->
      gen_tcp:close(Sock)
  end.

send_msg(Sock, Result, Description) ->
  ResBin = protocol:encode_msg(#{dest => User, type => "RESPONSE", response => #{result => Result, description => Description}}, 'Message'),
  gen_tcp:send(Sock, ResBin).

registarhandler(Sock, Room, User, Password) ->
  case login_manager:create_account(User, Password) of
    ok ->
      send_msg(Sock, "OK", "USER CREATED"),
      Room ! {enter, self()},
      user(User, Sock, Room);
    user_exists ->
      send_msg(Sock, "EXCEPTION", "USER EXISTS"),
      handleauth(Sock, Room)
  end.

loginhandler(Sock, Room, User, Password) ->
  case login_manager:login(User, Password) of
    ok ->
      send_msg(Sock, "OK", "LOGGED IN"),
      Room ! {enter, self()},
      user(User, Sock, Room);
    error ->
      send_msg(Sock, "EXCEPTION", "INVALID LOGIN"),
      handleauth(Sock, Room)
  end.

room(Pids,Rooms) ->
  receive
    {enter, Pid} ->
      io:format("user entered~n", []),
      room([Pid | Pids], Rooms);
    {line, Data} = Msg ->
      io:format("received ~p~n", [Data]),
      [Pid ! Msg || Pid <- Pids],
      room(Pids, Rooms);
    {leave, Pid} ->
      io:format("user left~n", []),
      room(Pids -- [Pid], Rooms);
    {room, N, PidU} ->
      case maps:find(N,Rooms) of
        error ->
          NR = spawn(fun()-> room([PidU],Rooms) end),
          NR ! {room_init, N, NR},
          PidU ! {room, NR},
          room(Pids -- [PidU], maps:put(N, NR, Rooms));
        {ok, Value} ->
          Value ! {enter, PidU},
          PidU ! {room, Value},
          room(Pids -- [PidU], Rooms)
      end;
    {room_init, N, PidChat} ->
      room(Pids, maps:put(N, PidChat, Rooms))
  end.

user(User, Sock, Room) ->
  receive
    {line, Data} ->
      gen_tcp:send(Sock, Data),
      user(User, Sock, Room);
    {tcp, _, Data} ->
      case string:tokens(binary_to_list(Data)," \r\n") of
        ["exit"] ->
          Room ! {leave, self()},
          login_manager:logout(User),
          gen_tcp:close(Sock);
        ["room", Name] ->
          Room ! {room, Name, self()},
          user(User, Sock, Room);
        _ ->
          Room ! {line, Data},
          user(User, Sock, Room)
      end;
    {tcp_closed, _} ->
      Room ! {leave, self()};
    {tcp_error, _, _} ->
      Room ! {leave, self()};
    {room, NR} ->
      user(User, Sock, NR)
  end.

