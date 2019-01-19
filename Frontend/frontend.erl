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
          registarhandler(Sock, User, Pass);
        "LOGIN" ->
          loginhandler(Sock, User, Pass)
      end;
    {tcp_closed, _} ->
      false;
    {tcp_error, _, _} ->
      gen_tcp:close(Sock)
  end.

send_msg(Sock, User, Type, Result, Description) ->
  ResBin = protocol:encode_msg(#{dest => User, type => Type, response => #{result => Result, description => Description}}, 'Message'),
  gen_tcp:send(Sock, ResBin).

registarhandler(Sock, User, Password) ->
  case login_manager:create_account(User, Password) of
    ok ->
      send_msg(Sock, User, "RESPONSE", "OK", "USER CREATED"),
      user_manager:user(User, Sock);
    user_exists ->
      send_msg(Sock, User, "RESPONSE", "EXCEPTION", "USER EXISTS"),
      handleauth(Sock)
  end.

loginhandler(Sock, User, Password) ->
  case login_manager:login(User, Password) of
    ok ->
      send_msg(Sock, User, "RESPONSE", "OK", "LOGGED IN"),
      user_manager:user(User, Sock);
    error ->
      send_msg(Sock, User, "RESPONSE", "EXCEPTION", "INVALID LOGIN"),
      handleauth(Sock)
  end.
