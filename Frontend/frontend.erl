-module(frontend).
-export([server/1]).

server(Port) ->
  exchange_manager:run(),
  login_manager:start(),
  {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
  acceptor(LSock).

acceptor(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock) end),
  handleauth(Sock).

handleauth(Sock) ->
  receive
    {tcp, _, Data} ->
      Msg = protocol:decode_msg(Data, 'Message'),
      Cli = maps:get(user, Msg),
      User = maps:get(username, Cli),
      Pass = maps:get(password, Cli),
      case maps:get(type, Msg) of
        "REGISTER" ->
          UserType = maps:get(userType, Msg),
          registarhandler(Sock, User, Pass, UserType);
        "LOGIN" ->
          loginhandler(Sock, User, Pass)
      end;
    {tcp_closed, _} ->
      false;
    {tcp_error, _, _} ->
      gen_tcp:close(Sock)
  end.

send_msg(Sock, User, Type, UserType, Result, Description) ->
  ResBin = protocol:encode_msg(#{dest => User, type => Type, userType => UserType, response => #{result => Result, description => Description}}, 'Message'),
  gen_tcp:send(Sock, ResBin).

registarhandler(Sock, User, Password, UserType) ->
  case login_manager:create_account(User, Password, UserType) of
    {ok, UT} ->
      send_msg(Sock, User, "RESPONSE", UT, "OK", "USER CREATED"),
      handleauth(Sock);
    {user_exists, UT} ->
      send_msg(Sock, User, "RESPONSE", UT, "EXCEPTION", "USER EXISTS"),
      handleauth(Sock)
  end.

loginhandler(Sock, User, Password) ->
  case login_manager:login(User, Password) of
    {ok, UT} ->
      send_msg(Sock, User, "RESPONSE", UT, "OK", "LOGGED IN"),
      user_manager:user(User, Sock);
    {error, UT} ->
      send_msg(Sock, User, "RESPONSE", UT, "EXCEPTION", "INVALID LOGIN"),
      handleauth(Sock)
  end.
