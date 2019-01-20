package directory.models;

import org.hibernate.validator.constraints.NotEmpty;

public class Auction {
    @NotEmpty
    final private int id;
    @NotEmpty
    final private long amount;
    @NotEmpty
    final private float maxRate;

    public Auction(int id, long amount, float maxRate) {
        this.id = id;
        this.amount = amount;
        this.maxRate = maxRate;
    }

    public int getId() {
        return id;
    }

    public long getAmount() {
        return amount;
    }

    public float getMaxRate() {
        return maxRate;
    }
}
