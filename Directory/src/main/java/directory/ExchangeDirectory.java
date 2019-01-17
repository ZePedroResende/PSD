package directory;

import io.dropwizard.Application;
import io.dropwizard.Configuration;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

public class ExchangeDirectory extends Application<Configuration> {
    public static void main(String[] args) throws Exception {
        new ExchangeDirectory().run(args);
    }

    @Override
    public void initialize(Bootstrap<Configuration> bootstrap) {}

    @Override
    public void run(Configuration exchangeDirectoryConfiguration, Environment environment) throws Exception {
        environment.jersey().register();
    }
}