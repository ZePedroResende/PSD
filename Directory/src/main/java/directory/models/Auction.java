package directory.models;

import java.util.HashMap;
import java.util.Map;

public class Auction {
    final private int id;
    final private String company;
    final private float maxRate;
    final private long amount;
    final private Map<String, Bid> bids;

    private Boolean active;
    private Boolean success;

    public Auction(int id, String company, long amount, float maxRate) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.maxRate = maxRate;
        this.bids = new HashMap<>();
        this.setActive(true);
        this.setSuccess(false);
    }

    public int getId() {
        return id;
    }

    public String getCompany() {
        return company;
    }

    public float getMaxRate() {
        return maxRate;
    }

    public long getAmount() {
        return amount;
    }

    public Map<String, Bid> getBids() {
        return bids;
    }

    public boolean isActive() {
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    public boolean isSuccess() {
        return success;
    }

    public void setSuccess(boolean success) {
        this.success = success;
    }
}
