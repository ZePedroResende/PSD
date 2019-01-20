package directory.models;

public class Emission {
    final private String company;
    final private int id;
    final private Float rate;
    final private Long amount;

    private boolean active;
    private boolean success;

    public Emission(String company, int id, long amount, float rate) {
        this.company = company;
        this.id = id;
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

    public Float getRate() {
        return rate;
    }

    public Long getAmount() {
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
