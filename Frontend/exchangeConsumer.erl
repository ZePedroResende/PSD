-module(exchangeConsumer).
-export([run/0]).

run() ->
    {ok, Context} = erlzmq:context(),
    {ok, Sock} = erlzmq:socket(Context, [pull, {active, false}]),
    ok = erlzmq:connect(Sock, "tcp://localhost:3332"),
    spawn( fun() -> exchangeConsumer(Sock) end).


exchangeConsumer(Sock) ->
	case erlzmq:recv(Sock) of
		{ok, Bin} ->
			Msg = protocol:decode_msg(Bin, 'Message'),
			io:format("Incoming pull message: ~p\n", [Msg]),
			User = maps:get(user, Msg),
			Username = maps:get(username, User),
			case login_manager:user_online(Username) of
				error -> 
					exchangeConsumer(Sock);
				Pid ->
					user_manager:send_msg(Pid, Bin),
					exchangeConsumer(Sock)
			end;
		{error, _} -> 
			exchangeConsumer(Sock)
	end.
