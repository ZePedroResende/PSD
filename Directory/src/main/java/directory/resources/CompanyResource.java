package directory.resources;

import com.codahale.metrics.annotation.Timed;
import directory.models.Company;
import directory.models.Exchange;
import directory.representation.Representation;
import directory.services.CompanyService;
import org.eclipse.jetty.http.HttpStatus;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/companies")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class CompanyResource {
    private final CompanyService service;

    public CompanyResource(@NotNull CompanyService service) {
        this.service = service;
    }


    @GET
    @Timed
    @Path("/")
    public Representation<List<Company>> indexCompanies() {
        return new Representation<>(HttpStatus.OK_200, service.getCompanies());
    }

    @GET
    @Timed
    @Path("{name}")
    public Representation<Company> showCompany(@NotNull @PathParam("name") final String name) {
        return new Representation<>(HttpStatus.OK_200, service.getCompany(name));
    }

    @GET
    @Timed
    @Path("{name}/exchange")
    public Representation<Exchange> showCompanyExchange(@NotNull @PathParam("name") final String name) {
        return new Representation<>(HttpStatus.OK_200, service.getCompanyExchange(name));
    }

    @POST
    @Timed
    @Path("/")
    public Representation<Company> createCompany(@NotNull @Valid final Company company) {
        return new Representation<>(HttpStatus.OK_200, service.createCompany(company));
    }
}
