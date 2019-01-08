public class Buy {
    private Long offer;
    private Integer userId;

    public Buy(Long offer, Integer userId) {
        this.offer = offer;
        this.userId = userId;
    }

    public Long getOffer() {
        return offer;
    }

    public Integer getUserId() {
        return userId;
    }
}
