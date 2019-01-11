import com.sun.org.apache.xpath.internal.operations.Bool;

import java.util.*;

public class Auction implements Sale{
    private Map<Integer,Bid> bids;
    private Long amount;
    private Float maxRate;
    private Boolean active;
    private Integer companyId;
    private Boolean sucess;

    public Auction( Long maxAmount, Float maxRate, Integer companyId, Integer time) {
        this.bids = new HashMap<>();
        this.amount = maxAmount;
        this.maxRate = maxRate;
        this.active = true;
        this.companyId = companyId;
        new Timer().schedule(new Finisher(this),time);
    }

    public Map<Integer,Bid> getBid() {
        return bids;
    }

    public Long getMaxAmount() {
        return amount;
    }

    public Float getMaxRate() {
        return maxRate;
    }

    public synchronized boolean addBid(Bid bid){
        boolean valid = this.maxRate > bid.getRate() && active;
        if(valid){
            this.bids.put(bid.getUserId(),bid);
        }
        return valid;
    }

    public Integer getCompanyId() {
        return companyId;
    }

    public synchronized Boolean isActive(){
        return this.active;
    }

    public synchronized Boolean isSucess(){
        return this.sucess;
    }

    public synchronized Map.Entry<Boolean, List<List<Bid>>> getResults() throws InterruptedException {
        while (active){
            wait();
        }

        ArrayList<Bid> bids = new ArrayList<>(this.bids.values());
        bids.sort((Bid x, Bid y) -> Math.toIntExact(x.getOffer() - y.getOffer()));
        int index = 0;
        int value = 0;

        for (Bid b : bids) {
            value += b.getOffer();
            if (value >= this.amount) break;
            index++;
        }

        List<List<Bid>> result = new ArrayList<>();

        if(index == 0) {
            result.add(new ArrayList<>());
            result.add(bids);
        }
        else if (index == bids.size()) {
            result.add(bids);
            result.add(new ArrayList<Bid>());
        } else {
            result.add(bids.subList(0, index));
            result.add(bids.subList(index + 1, bids.size() - 1));
        }

        this.sucess = value >= this.amount;
        return new AbstractMap.SimpleEntry<>(sucess, result);
    }

    public Float getMinimalRate(){
        try {
            Map.Entry<Boolean, List<List<Bid>>> results = getResults();

            List<Bid> bids = results.getValue().get(0);

            if(bids.size() > 0){
                return bids.get(bids.size()-1).getRate();
            }
            else {
                return new Float(-1);
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        return new Float(-2);
    }

    private void finishAuction(){
        this.active = false;
        notifyAll();
    }

    private class Finisher extends TimerTask{
        private Auction auction;

        Finisher(Auction auction){
            this.auction = auction;
        }
        @Override
        public void run() {
            auction.finishAuction();
        }
    }

}

