package directory.resources;

import com.codahale.metrics.annotation.Timed;
import directory.models.Auction;
import directory.models.Emission;
import directory.representation.Representation;
import directory.services.AuctionService;
import directory.services.EmissionService;
import org.eclipse.jetty.http.HttpStatus;

import javax.validation.constraints.NotNull;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/auctions")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class AuctionResource {
    private final AuctionService service;

    public AuctionResource(@NotNull AuctionService service) {
        this.service = service;
    }

    @GET
    @Timed
    @Path("/")
    public Representation<List<Auction>> indexAuctions() {
        return new Representation<>(HttpStatus.OK_200, service.getAuctions());
    }

    @GET
    @Timed
    @Path("{id}")
    public Representation<Auction> showAuction(@NotNull @PathParam("id") final long id) {
        return new Representation<>(HttpStatus.OK_200, service.getAuction(id));
    }

    @POST
    @Timed
    @Path("/")
    public Representation<Auction> createAuction(
            @NotNull @QueryParam("company") final String company,
            @NotNull @QueryParam("amount") final long amount,
            @NotNull @QueryParam("maxrate") final float maxRate
    ) {
        return new Representation<>(HttpStatus.OK_200, service.createAuction(company, amount, maxRate));
    }

    @PUT
    @Timed
    @Path("{id}")
    public Representation<Auction> updateAuction(
            @NotNull @PathParam("id") final long id,
            @NotNull @QueryParam("active") final boolean active,
            @NotNull @QueryParam("success") final boolean success
    ) {
        return new Representation<>(HttpStatus.OK_200, service.updateAuction(id, active, success));
    }
}
