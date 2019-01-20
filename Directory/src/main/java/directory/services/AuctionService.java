package directory.services;

import directory.models.Auction;
import directory.models.Emission;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class AuctionService {
    public static final String AUCTION_NOT_FOUND = "Auction %s not found";

    private final Map<Integer, Auction> auctions;
    private final AtomicInteger counter;

    public AuctionService() {
        this.auctions = new HashMap<>();
        this.counter = new AtomicInteger();
    }

    public List<Auction> getAuctions() {
        return new ArrayList<>(this.auctions.values());
    }

    public Auction getAuction(final int id) {
        Auction auction = auctions.get(id);

        if (auction == null) {
            final String errorMessage = String.format(AUCTION_NOT_FOUND, id);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        return auction;
    }

    public Auction createAuction(String company, long amount, float maxRate) {
        final int id = counter.incrementAndGet();

        return auctions.put(id, new Auction(id, company, amount, maxRate));
    }

    public Auction updateAuction(int id, boolean active, boolean success) {
        Auction auction = auctions.get(id);

        if (auction == null) {
            final String errorMessage = String.format(AUCTION_NOT_FOUND, id);
            throw new WebApplicationException(errorMessage, Response.Status.NOT_FOUND);
        }

        auction.setActive(active);
        auction.setSuccess(success);

        return auctions.put(id, auction);
    }
}
