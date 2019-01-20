package directory.services;

import directory.models.Company;
import directory.models.Exchange;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class DirectoryService {
    private static final String EXCHANGE_NOT_FOUND = "Exchange %s does not exist";
    private static final String EXCHANGE_ALREADY_EXISTS = "Exchange %s already exists";
    private static final String COMPANY_NOT_FOUND = "Company %s does not exist";
    private static final String COMPANY_ALREADY_EXISTS = "Company %s already exists";

    private final HashMap<String, Company> companies;
    private final HashMap<String, Exchange> exchanges;

    public DirectoryService() {
        this.exchanges = new HashMap<>();
        this.companies = new HashMap<>();

        populateDirectory();
    }

    private void populateDirectory() {
        Exchange exch1 = new Exchange("exch1", "localhost", 5431);
        Exchange exch2 = new Exchange("exch2", "localhost", 5432);
        Exchange exch3 = new Exchange("exch3", "localhost", 5433);

        exchanges.put(exch1.getName(), exch1);
        exchanges.put(exch2.getName(), exch2);
        exchanges.put(exch3.getName(), exch3);

        companies.put("Google", new Company("Google", exch1));
        companies.put("Apple", new Company("Apple", exch1));
        companies.put("Microsoft", new Company("Microsoft", exch1));
        companies.put("Amazon", new Company("Amazon", exch2));
        companies.put("Tesla", new Company("Tesla", exch2));
        companies.put("Netflix", new Company("Netflix", exch3));
        companies.put("Disney", new Company("Disney", exch3));
        companies.put("Oracle", new Company("Oracle", exch3));
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

    public List<Company> getCompanies() {
        return new ArrayList<>(this.companies.values());
    }

    public Company getCompany(String name) {
        Company company = companies.get(name);

        if (company == null) {
            final String errorMessage = String.format(COMPANY_NOT_FOUND, name);
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
            final String errorMessage = String.format(COMPANY_ALREADY_EXISTS, key);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_ACCEPTABLE);
        }

        companies.put(key, company);

        return company;
    }
}
