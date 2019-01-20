package directory;

import directory.health.TemplateHealthCheck;
import directory.core.Template;
import directory.resources.CompanyResource;
import directory.resources.ExchangeResource;
import directory.services.CompanyService;
import directory.services.ExchangeService;
import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

public class PeerLendingDirectory extends Application<DirectoryConfiguration> {
    public static void main(String[] args) throws Exception {
        new PeerLendingDirectory().run(args);
    }

    @Override
    public String getName() {
        return "peer-lending-directory";
    }

    @Override
    public void initialize(Bootstrap<DirectoryConfiguration> bootstrap) {}

    @Override
    public void run(DirectoryConfiguration configuration, Environment environment) {
        final ExchangeService exchangeService = new ExchangeService();
        final CompanyService companyService = new CompanyService(exchangeService);

        exchangeService.populateDirectory();
        companyService.populateDirectory(exchangeService.getExchanges());

        final ExchangeResource exchangeResource = new ExchangeResource(exchangeService);
        final CompanyResource companyResource = new CompanyResource(companyService);

        final Template template = configuration.buildTemplate();

        environment.healthChecks().register("template", new TemplateHealthCheck(template));
        environment.jersey().register(exchangeResource);
        environment.jersey().register(companyResource);
    }
}