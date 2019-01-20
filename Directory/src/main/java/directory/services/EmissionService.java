package directory.services;

import directory.models.Company;
import directory.models.Emission;
import directory.models.Exchange;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class EmissionService {
    public static final String EMISSION_NOT_FOUND = "Emission %s not found";

    private final Map<Integer, Emission> emissions;
    private final AtomicInteger counter;

    public EmissionService() {
        this.emissions = new HashMap<>();
        this.counter = new AtomicInteger();
    }

    public List<Emission> getEmissions() {
        return new ArrayList<>(this.emissions.values());
    }

    public Emission getEmission(final int id) {
        Emission emission = emissions.get(id);

        if (emission == null) {
            final String errorMessage = String.format(EMISSION_NOT_FOUND, id);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return emission;
    }

    public Emission createEmission(String company, long amount, float rate) {
        final int id = counter.incrementAndGet();

        return emissions.put(id, new Emission(company, id, amount, rate));
    }

    public Emission updateEmission(int id, boolean active, boolean success) {
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
