package directory.services;

import directory.models.Exchange;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ExchangeService {
    private static final String EXCHANGE_NOT_FOUND = "Exchange %s does not exist";
    private static final String EXCHANGE_ALREADY_EXISTS = "Exchange %s already exists";

    private final Map<String, Exchange> exchanges;

    public ExchangeService() {
        this.exchanges = new HashMap<>();
    }

    public List<Exchange> getExchanges() {
        return new ArrayList<>(this.exchanges.values());
    }

    public Exchange getExchange(String name) {
        Exchange exchange = exchanges.get(name);

        if (exchange == null) {
            final String errorMessage = String.format(EXCHANGE_NOT_FOUND, name);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return exchange;
    }

    public Exchange createExchange(Exchange exchange) {
        final String key = exchange.getName();
        final boolean exists = exchanges.containsKey(key);

        // Request validation
        if (exists) {
            final String errorMessage = String.format(EXCHANGE_ALREADY_EXISTS, key);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_ACCEPTABLE);
        }

        exchanges.put(key, exchange);

        return exchange;
    }

    public void populateDirectory() {
        Exchange exch1 = new Exchange("exch1", "localhost", 5431);
        Exchange exch2 = new Exchange("exch2", "localhost", 5432);
        Exchange exch3 = new Exchange("exch3", "localhost", 5433);

        exchanges.put(exch1.getName(), exch1);
        exchanges.put(exch2.getName(), exch2);
        exchanges.put(exch3.getName(), exch3);
    }
}
