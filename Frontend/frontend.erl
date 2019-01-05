-module(chatv3).
-export([server/1]).

server(Port) ->
  Room = spawn(fun()-> room([],#{}) end),
  Room ! {room_init, "default", Room},
  login_manger:start(),
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

registarhandler(Sock, Room, User, Password) ->
  case login_manger:create_account(User, Password) of
    ok ->
      ResBin = protocol:encode_msg(#{dest => User, type => "RESPONSE", response => #{result => "OK", description => "SUCCESSFUL REGISTER"}}, 'Message'),
      gen_tcp:send(Sock, ResBin),
      Room ! {enter, self()},
      user(User, Sock, Room);
    user_exists ->
      ResBin = protocol:encode_msg(#{dest => User, type => "RESPONSE", response => #{result => "EXCEPTION", description => "USER EXISTS"}}, 'Message'),
			handleauth(Sock, Room)
  end.

loginhandler(Sock, Room, User, Password) ->
  case login_manger:login(User, Password) of
    ok ->
      ResBin = protocol:encode_msg(#{dest => User, type => "RESPONSE", response => #{result => "EXCEPTION", description => "INVALID LOGIN"}}, 'Message'),
      gen_tcp:send(Sock, ResBin),
      Room ! {enter, self()},
      user(User, Sock, Room);
    error ->
      false
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
      SplitData = string:split(Data, " "),
      case string:equal("join",lists:nth(1, SplitData)) of
        false ->
          case string:equal("exit\r\n", Data) of
            true ->
              Room ! {leave, self()},
              login_manger:logout(User),
              gen_tcp:close(Sock);
            false -> 
              Room ! {line, Data},
              user(User, Sock, Room)
          end;
        true ->
          Room ! {room, lists:nth(2, SplitData), self()},
          user(User, Sock, Room)
      end;
    {tcp_closed, _} ->
      Room ! {leave, self()};
    {tcp_error, _, _} ->
      Room ! {leave, self()};
    {room, NR} ->
      user(User, Sock, NR)
  end.

