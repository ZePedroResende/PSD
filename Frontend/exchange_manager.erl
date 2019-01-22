-module(exchange_manager).
-export([run/0, find_producer/1]).

run() ->
    exchangeConsumer:run(),
    register(?MODULE, spawn(fun() -> exchange(#{},#{}) end)).

find_producer(Company) ->
    ?MODULE ! {find, Company, self()},
    receive
        Res -> Res
    end.

exchange(Exchanges, Cache) ->
    receive
        {find, Company, From} ->
            case maps:find(Company, Cache) of
                {ok, Pid} ->
                    From ! {ok, Pid},
                    exchange(Exchanges, Cache);
                error ->
                    Info = findCompany(Company),
                    Name = maps:get(name, Info),
                    case maps:find(Name, Exchanges) of
                        {ok, Pid} ->
                            maps:put(Company, Pid, Cache),
                            From ! {ok, Pid},
                            exchange(Exchanges, Cache);
                        error ->
                            Host = maps:get(host, Info),
                            Port = maps:get(port, Info),
                            PidE = exchangeProducer:run(Host,Port),
                            maps:put(Name, PidE, Exchanges),
                            maps:put(Company, PidE, Cache),
                            From ! {ok, PidE},
                            exchange(Exchanges, Cache)
                    end
            end
    end.

findCompany(Company) ->
    inets:start(),
    io:format("data2 "),
    case httpc:request("http://localhost:8080/companies/" ++ Company) of
        {ok, {_, _, Result}} ->
            inets:stop(),
            {struct, Json} = mochijson:decode(Result),
	        {_, Data} = proplists:get_value("data", Json), 
            {_, Exchange} = proplists:get_value("exchange", Data), 
	        proplists_to_map(Exchange);
        _ ->
            inets:stop(),
            io:format("erro ")
    end.


proplists_to_map(Exchange) ->
	Name = proplists:get_value("name", Exchange),
	Host = proplists:get_value("host", Exchange),
	Port = proplists:get_value("port", Exchange),
	#{name => Name, host => Host, port => Port}.