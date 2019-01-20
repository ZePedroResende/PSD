package directory.models;

import org.hibernate.validator.constraints.NotEmpty;

public class Bid {
    @NotEmpty
    final private long offer;
    @NotEmpty
    final private float rate;
    @NotEmpty
    final private String user;

    public Bid(long offer, float rate, String user) {
        this.offer = offer;
        this.rate = rate;
        this.user = user;
    }


    public long getOffer() {
        return offer;
    }

    public float getRate() {
        return rate;
    }

    public String getUser() {
        return user;
    }
}
