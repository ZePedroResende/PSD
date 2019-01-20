package directory.models;

import org.hibernate.validator.constraints.NotEmpty;

import java.util.ArrayList;
import java.util.List;

public class Company {
    @NotEmpty
    private final String name;
    @NotEmpty
    private final Exchange exchange;
    @NotEmpty
    private final List<Auction> auctions;
    @NotEmpty
    private final List<Emission> emissions;

    public Company(String name, Exchange exchange) {
        this.name = name;
        this.exchange = exchange;
        this.auctions = new ArrayList<>();
        this.emissions = new ArrayList<>();
    }

    public String getName() {
        return name;
    }

    public Exchange getExchange() {
        return exchange;
    }

    public List<Auction> getAuctions() {
        return auctions;
    }

    public List<Emission> getEmissions() {
        return emissions;
    }
}
