package directory.models;

import org.hibernate.validator.constraints.NotEmpty;

public class Emission {
    @NotEmpty
    final private int id;
    @NotEmpty
    final private String company;
    @NotEmpty
    final private float rate;
    @NotEmpty
    final private long amount;

    private boolean active;
    private boolean success;

    public Emission(int id, String company, long amount, float rate) {
        this.id = id;
        this.company = company;
        this.amount = amount;
        this.rate = rate;
        this.setActive(true);
        this.setSuccess(false);
    }


    public String getCompany() {
        return company;
    }

    public int getId() {
        return id;
    }

    public float getRate() {
        return rate;
    }

    public long getAmount() {
        return amount;
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
