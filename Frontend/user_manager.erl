-module(user_manager).
-export([user/2, send_msg/2]).

send_msg(User, Data) -> 
    User ! {send, Data}.

user(User, Sock) ->
  receive
    {send, Data} ->
      gen_tcp:send(Sock, Data),
      user(User, Sock);
    {tcp, Sock, Data} ->
      Msg = protocol:decode_msg(Data, 'Message'),
      Sale = maps:get(sale, Msg),
			Company = maps:get(name, Sale),
      {ok, Pid} = exchange_manager:find_producer(Company),
      io:format("Message = ~p\n", [Msg]),
			ok = exchangeProducer:new_order(Data, Pid),
			user(User, Sock);
    {tcp_closed, Sock} ->
      login_manager:logout(User),
      exit(normal);
    {tcp_error, _, _} ->
      login_manager:logout(User),
      exit(normal)
  end.
