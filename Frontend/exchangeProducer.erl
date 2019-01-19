-module(exchangeProducer).
-export([run/2, new_order/2]).

run(Host, Port) ->
	{ok, Context} = erlzmq:context(1),
	{ok, Sock} = erlzmq:socket(Context, [push, {active, false}]),
	ok = erlzmq:connect(Sock, "tcp://" ++ Host ++ ":" ++ integer_to_list(Port)),
	io:format("ZMQ connect " ++ Host ++ ":" ++ integer_to_list(Port)),
	spawn( fun() -> exchangeProducer(Sock) end).

new_order(Msg, Pid) ->
	Pid ! {order, Msg, self()},
	io:format("I, ~p, sent a message to ~p\n", [self(), Pid]),
	receive
		Rep -> Rep
	end.

exchangeProducer(Sock) ->
	receive
		{order, Msg, From} ->
			io:format("I, ~p, got a message from ~p\n", [self(), From]),
			case erlzmq:send(Sock, Msg) of
				ok -> From ! ok;
				_ -> From ! error
			end,
			io:format("Ready for another one!\n"),
			exchangeProducer(Sock)
	end.