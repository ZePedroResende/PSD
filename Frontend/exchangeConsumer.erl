-module(exchangeConsumer).
-export([run/0]).

run() ->
    {ok, Context} = erlzmq:context(),
    {ok, Sock} = erlzmq:socket(Context, [pull, {active, false}]),
    register(?MODULE, spawn ( fun() -> exchangeConsumer(Sock) end) ).


exchangeConsumer(Sock) ->
	case erlzmq:recv(Sock) of
		{ok, Bin} ->
			Msg = protocol:decode_msg(Bin, 'Message'),
			io:format("Incoming message: ~p\n", [Msg]),
			Pid = maps:get(pid, Msg),
			user_manager:send(Msg, Pid),
			exchangeConsumer(Sock);
		{error, _} -> 
			exchangeConsumer(Sock)
	end.