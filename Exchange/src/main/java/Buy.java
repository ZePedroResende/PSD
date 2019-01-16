package main.java;

public class Buy {
    private Long offer;
    private String user;

    public Buy(Long offer, String user) {
        this.offer = offer;
        this.user = user;
    }

    public Long getOffer() {
        return offer;
    }

    public String getUser() {
        return user;
    }
}
