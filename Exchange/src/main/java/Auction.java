import org.zeromq.ZMQ;

import java.util.*;
import Protos.Protocol;

public class Auction implements Sale {
    private Map<String,Bid> bids;
    private int id;
    private Long amount;
    private Float maxRate;
    private Boolean active;
    private String company;
    private Boolean sucess;
    private ZMQ.Socket push;

    public Auction(Long maxAmount, Float maxRate, String company, int id, Integer time, ZMQ.Socket push) {
        this.id = id;
        this.bids = new HashMap<>();
        this.amount = maxAmount;
        this.maxRate = maxRate;
        this.active = true;
        this.company = company;
        this.push = push;
        new Timer().schedule(new Finisher(this),time);
    }

    public Map<String,Bid> getBid() {
        return bids;
    }

    public int getId(){
        return id;
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
            this.bids.put(bid.getUser(),bid);
        }
        return valid;
    }

    public String getCompany() {
        return company;
    }

    public synchronized Boolean isActive(){
        return this.active;
    }

    public synchronized Boolean isSucess(){
        return this.sucess;
    }

    public synchronized Map.Entry<Boolean, List<List<Bid>>> getResults()  {
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

    public synchronized Float getMinimalRate(){
            Map.Entry<Boolean, List<List<Bid>>> results = getResults();

            List<Bid> bids = results.getValue().get(0);

            if(bids.size() > 0){
                return bids.get(bids.size()-1).getRate();
            }
            else {
                return new Float(-1);
            }

    }

    private void finishAuction(){
        this.active = false;

        Map.Entry<Boolean, List<List<Bid>>> result = getResults();
        List<Bid> winner = result.getValue().get(0);
        List<Bid> loser = result.getValue().get(1);

        System.out.println("winner: " + winner.size() + " loser:"+loser.size());
        String strResult = result.getKey() ? "SUCESS" : "FAILURE";
        Protocol.State state1 = Protocol.State.newBuilder().setResult(strResult).setDescription("AUCTION" + this.id + " " + result.getKey()).build();
        Protocol.User user1 = Protocol.User.newBuilder().setUsername(company).build();
        Protocol.Message response1 = Protocol.Message.newBuilder().setState(state1).setUser(user1).build();
        push.send(response1.toByteArray());

        winner.forEach(x -> {
            Protocol.State state = Protocol.State.newBuilder().setResult("WON").setDescription("AUCTION" + this.id + " " + result.getKey()).build();
            Protocol.User user = Protocol.User.newBuilder().setUsername(x.getUser()).build();
            Protocol.Sale sale = Protocol.Sale.newBuilder().setRate(x.getRate()).setValue(x.getOffer()).build();
            Protocol.Message response = Protocol.Message.newBuilder().setState(state).setSale(sale).setUser(user).build();
            push.send(response.toByteArray());

        });

        loser.forEach(x -> {
            Protocol.State state = Protocol.State.newBuilder().setResult("LOST").setDescription("AUCTION" + this.id + " " + result.getKey()).build();
            Protocol.User user = Protocol.User.newBuilder().setUsername(x.getUser()).build();
            Protocol.Sale sale = Protocol.Sale.newBuilder().setName(this.company).setRate(x.getRate()).setValue(x.getOffer()).build();
            Protocol.Message response = Protocol.Message.newBuilder().setState(state).setSale(sale).setUser(user).build();
            push.send(response.toByteArray());

        });

    }

    private class Finisher extends TimerTask{
        private Auction auction;

        Finisher(Auction auction){
            this.auction = auction;
        }
        @Override
        public void run() {
            System.out.println("FINISH auction: " + auction.id);
            auction.finishAuction();
        }
    }

}

