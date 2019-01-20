package directory.resources;

import com.codahale.metrics.annotation.Timed;
import directory.models.Company;
import directory.models.Exchange;
import directory.representation.Representation;
import directory.services.DirectoryService;
import org.eclipse.jetty.http.HttpStatus;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Collection;
import java.util.List;

@Path("/")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class DirectoryResource {
    private final DirectoryService service;

    public DirectoryResource(@NotNull DirectoryService service) {
        this.service = service;
    }

    @GET
    @Timed
    @Path("/exchanges")
    public Representation<List<Exchange>> indexExchanges() {
        return new Representation<>(HttpStatus.OK_200, service.getExchanges());
    }

    @GET
    @Timed
    @Path("/exchanges/{name}")
    public Representation<Exchange> showExchange(@NotNull @PathParam("name") final String name) {
        return new Representation<>(HttpStatus.OK_200, service.getExchange(name));
    }

    @POST
    @Timed
    @Path("/exchanges")
    public Representation<Exchange> createExchange(@NotNull @Valid final Exchange exchange) {
        return new Representation<>(HttpStatus.OK_200, service.createExchange(exchange));
    }

    @GET
    @Timed
    @Path("/companies")
    public Representation<List<Company>> indexCompanies() {
        return new Representation<>(HttpStatus.OK_200, service.getCompanies());
    }

    @GET
    @Path("/companies/{name}")
    public Representation<Company> showCompany(@NotNull @PathParam("name") final String name) {
        return new Representation<>(HttpStatus.OK_200, service.getCompany(name));
    }

    @GET
    @Path("/companies/{name}/exchange")
    public Representation<Exchange> showCompanyExchange(@NotNull @PathParam("name") final String name) {
        return new Representation<>(HttpStatus.OK_200, service.getCompanyExchange(name));
    }

    @POST
    @Timed
    @Path("/companies")
    public Representation<Company> createExchange(@NotNull @Valid final Company company) {
        return new Representation<>(HttpStatus.OK_200, service.createCompany(company));
    }
}
