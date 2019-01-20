
public class Bid extends Buy {
    private Float rate;

    public Bid(Long offer, Float rate, String user) {
       super(offer,user);
       this.rate = rate;
    }

    public Float getRate() {
        return rate;
    }
}
