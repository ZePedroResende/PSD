package directory.services;

import directory.models.Auction;
import directory.models.Company;
import directory.models.Emission;
import directory.models.Exchange;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CompanyService {
    public static final String COMPANY_NOT_FOUND = "Company %s does not exist";
    public static final String COMPANY_ALREADY_EXISTS = "Company %s already exists";

    private final ExchangeService exchangeService;
    private final AuctionService auctionService;
    private final EmissionService emissionService;
    private final Map<String, Company> companies;

    public CompanyService(
            ExchangeService exchangeService,
            AuctionService auctionService,
            EmissionService emissionService) {
        this.exchangeService = exchangeService;
        this.auctionService = auctionService;
        this.emissionService = emissionService;
        this.companies = new HashMap<>();
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

    public List<Auction> getCompanyAuctions(String name) {
        Company company = companies.get(name);

        if (company == null) {
            final String errorMessage = String.format("Company %s does not exist", name);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return company.getAuctions();
    }

    public List<Emission> getCompanyEmissions(String name) {
        Company company = companies.get(name);

        if (company == null) {
            final String errorMessage = String.format("Company %s does not exist", name);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return company.getEmissions();
    }

    public Company createCompany(String key, String exchange) {
        final boolean exists = companies.containsKey(key);

        // Request validation
        if (exists) {
            final String errorMessage = String.format(COMPANY_ALREADY_EXISTS, key);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_ACCEPTABLE);
        }

        Company company = new Company(key, exchangeService.getExchange(exchange));

        companies.put(key, company);

        return company;
    }

    public Company addCompanyAuction(String key, int auctionId) {
        Company company = companies.get(key);

        if (company == null) {
            final String errorMessage = String.format(COMPANY_NOT_FOUND, key);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        company.addAuction(auctionService.getAuction(auctionId));

        companies.put(key, company);

        return company;
    }

    public Company addCompanyEmission(String key, int emissionId) {
        Company company = companies.get(key);

        if (company == null) {
            final String errorMessage = String.format(COMPANY_NOT_FOUND, key);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        company.addEmission(emissionService.getEmission(emissionId));

        companies.put(key, company);

        return company;
    }

    public void populateDirectory() {
        final List<Exchange> exchanges = exchangeService.getExchanges();

        Exchange exch1 = exchanges.get(0);
        Exchange exch2 = exchanges.get(1);
        Exchange exch3 = exchanges.get(2);

        companies.put("Google", new Company("Google", exch1));
        companies.put("Apple", new Company("Apple", exch1));
        companies.put("Microsoft", new Company("Microsoft", exch1));
        companies.put("Amazon", new Company("Amazon", exch2));
        companies.put("Tesla", new Company("Tesla", exch2));
        companies.put("Netflix", new Company("Netflix", exch3));
        companies.put("Disney", new Company("Disney", exch3));
        companies.put("Oracle", new Company("Oracle", exch3));
    }
}
