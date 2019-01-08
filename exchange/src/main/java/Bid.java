public class Bid extends Buy {
    private Integer rate;

    public Bid(Long offer, Integer rate, Integer userId) {
       super(offer,userId);
       this.rate = rate;
    }

    public Integer getRate() {
        return rate;
    }
}
