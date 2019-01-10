public class Bid extends Buy {
    private Float rate;

    public Bid(Long offer, Float rate, Integer userId) {
       super(offer,userId);
       this.rate = rate;
    }

    public Float getRate() {
        return rate;
    }
}
