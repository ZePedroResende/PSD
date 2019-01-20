package directory.services;

import directory.models.Emission;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

public class EmissionService {
    public static final String EMISSION_NOT_FOUND = "Emission %s not found";

    private final Map<Long, Emission> emissions;
    private AtomicLong counter;

    public EmissionService() {
        this.emissions = new HashMap<>();
        this.counter = new AtomicLong();
    }

    public List<Emission> getEmissions() {
        return new ArrayList<>(this.emissions.values());
    }

    public Emission getEmission(long id) {
        Emission emission = emissions.get(id);

        if (emission == null) {
            final String errorMessage = String.format(EMISSION_NOT_FOUND, id);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return emission;
    }

    public Emission createEmission(String company, long amount, float rate) {
        final long id = counter.incrementAndGet();

        Emission emission = new Emission(id, company, amount, rate);

        emissions.putIfAbsent(id, emission);

        return emission;
    }

    public Emission updateEmission(long id, boolean active, boolean success) {
        Emission emission = emissions.get(id);

        if (emission == null) {
            final String errorMessage = String.format(EMISSION_NOT_FOUND, id);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        emission.setActive(active);
        emission.setSuccess(success);

        return emissions.put(id, emission);
    }
}
