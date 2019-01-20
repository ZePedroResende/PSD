package directory.services;

import directory.models.Company;
import directory.models.Exchange;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class DirectoryService {
    private final HashMap<String, Company> companies;
    private final HashMap<String, Exchange> exchanges;

    public DirectoryService() {
        this.exchanges = new HashMap<>();
        this.companies = new HashMap<>();
    }

    public List<Exchange> getExchanges() {
        return new ArrayList<>(this.exchanges.values());
    }

    public Exchange getExchange(String name) {
        Exchange exchange = exchanges.get(name);

        if (exchange == null) {
            final String errorMessage = String.format("Exchange %s does not exist", name);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return exchange;
    }

    public Exchange createExchange(Exchange exchange) {
        final String key = exchange.getName();
        final boolean exists = exchanges.containsKey(key);

        // Request validation
        if (exists) {
            final String errorMessage = String.format("Exchange %s already exists", key);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_ACCEPTABLE);
        }

        exchanges.put(key, exchange);

        return exchange;
    }

    public List<Company> getCompanies() {
        return new ArrayList<>(this.companies.values());
    }

    public Company getCompany(String name) {
        Company company = companies.get(name);

        if (company == null) {
            final String errorMessage = String.format("Company %s does not exist", name);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return company;
    }

    public Exchange getCompanyExchange(String name) {
        Company company = companies.get(name);

        if (company == null) {
            final String errorMessage = String.format("Company %s does not exist", name);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return company.getExchange();
    }

    public Company createCompany(Company company) {
        final String key = company.getName();
        final boolean exists = companies.containsKey(key);

        // Request validation
        if (exists) {
            final String errorMessage = String.format("Company %s already exists", key);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_ACCEPTABLE);
        }

        companies.put(key, company);

        return company;
    }
}
