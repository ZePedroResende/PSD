package directory.resources;

import com.codahale.metrics.annotation.Timed;
import directory.models.Emission;
import directory.representation.Representation;
import directory.services.EmissionService;
import org.eclipse.jetty.http.HttpStatus;

import javax.validation.constraints.NotNull;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/emissions")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class EmissionResource {
    private final EmissionService service;

    public EmissionResource(@NotNull EmissionService service) {
        this.service = service;
    }

    @GET
    @Timed
    @Path("/")
    public Representation<List<Emission>> indexEmissions() {
        return new Representation<>(HttpStatus.OK_200, service.getEmissions());
    }

    @GET
    @Timed
    @Path("{id}")
    public Representation<Emission> showEmission(@NotNull @PathParam("id") final long id) {
        return new Representation<>(HttpStatus.OK_200, service.getEmission(id));
    }

    @POST
    @Timed
    @Path("/")
    public Representation<Emission> createEmission(
            @NotNull @QueryParam("company") final String company,
            @NotNull @QueryParam("amount") final long amount,
            @NotNull @QueryParam("rate") final float rate
    ) {
        return new Representation<>(HttpStatus.OK_200, service.createEmission(company, amount, rate));
    }

    @PUT
    @Timed
    @Path("{id}")
    public Representation<Emission> updateEmission(
            @NotNull @PathParam("id") final long id,
            @NotNull @QueryParam("active") final boolean active,
            @NotNull @QueryParam("success") final boolean success
    ) {
        return new Representation<>(HttpStatus.OK_200, service.updateEmission(id, active, success));
    }
}
