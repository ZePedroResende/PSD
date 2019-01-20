package directory.resources;

import com.codahale.metrics.annotation.Timed;
import directory.models.Exchange;
import directory.representation.Representation;
import directory.services.ExchangeService;
import org.eclipse.jetty.http.HttpStatus;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;


@Path("/exchanges")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class ExchangeResource {
    private final ExchangeService service;

    public ExchangeResource(@NotNull ExchangeService service) {
        this.service = service;
    }

    @GET
    @Timed
    @Path("/")
    public Representation<List<Exchange>> indexExchanges() {
        return new Representation<>(HttpStatus.OK_200, service.getExchanges());
    }

    @GET
    @Timed
    @Path("{name}")
    public Representation<Exchange> showExchange(@NotNull @PathParam("name") final String name) {
        return new Representation<>(HttpStatus.OK_200, service.getExchange(name));
    }

    @POST
    @Timed
    @Path("/")
    public Representation<Exchange> createExchange(@NotNull @Valid final Exchange exchange) {
        return new Representation<>(HttpStatus.OK_200, service.createExchange(exchange));
    }
}
