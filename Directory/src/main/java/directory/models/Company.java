package directory.models;

import org.hibernate.validator.constraints.NotEmpty;

public class Company {
    @NotEmpty
    private final String name;
    @NotEmpty
    private final Exchange exchange;

    public Company(String name, Exchange exchange) {
        this.name = name;
        this.exchange = exchange;
    }

    public String getName() {
        return name;
    }

    public Exchange getExchange() {
        return exchange;
    }
}
