
import org.zeromq.ZMQ;

import java.util.*;

public class Emission implements Sale {
    private String company;
    private int id;
    private Float rate;
    private List<Buy> buys;
    private Long amount;
    private Boolean active;
    private Boolean sucess;
    private ZMQ.Socket push;

    public Emission(String company, int id, Long amount, Integer time, Float rate, ZMQ.Socket push) {
        this.company = company;
        this.id = id;
        this.buys = new ArrayList<>();
        this.amount = amount;
        this.active = true;
        this.sucess = false;
        this.push = push;
        this.rate = rate;
        new Timer().schedule(new Finisher(this),time);
    }

    public String getCompany() {
        return company;
    }

    public synchronized List<Buy> getBuys() {
        return buys;
    }

    public Long getAmount() {
        return amount;
    }

    public Float getRate() {
        return rate;
    }

    public synchronized Boolean isActive() {
        return active;
    }

    public synchronized Boolean isSucess() {
        return sucess;
    }

    public synchronized boolean addBuy(Buy buy){
       if(active){
           buys.add(buy);
       }
       return active;
    }

    public synchronized Map.Entry<Boolean,List<Buy>> getResults() {
        long value = this.buys.stream()
                .mapToLong(buy -> buy.getOffer())
                .reduce(0,(x,y)-> x + y);

        this.sucess = value >= amount;

        return new AbstractMap .SimpleEntry<>(sucess,buys);
    }

    public synchronized void finishEmission(){
        this.active = false;

        Map.Entry<Boolean, List<Buy>> result = getResults();
        List<Buy> winner = result.getValue();

        winner.forEach(x -> {
            Protocol.State state = Protocol.State.newBuilder().setResult("WON").setDescription("BUY" + this.id + " " + result.getKey()).build();
            Protocol.User user = Protocol.User.newBuilder().setUsername(x.getUser()).build();
            Protocol.Sale sale = Protocol.Sale.newBuilder().setRate(this.rate).setValue(x.getOffer()).build();
            Protocol.Message response = Protocol.Message.newBuilder().setState(state).setSale(sale).setUser(user).build();
            push.send(response.toByteArray());

        });

    }

    private class Finisher extends TimerTask {
        private Emission emission;

        Finisher(Emission emission){
            this.emission = emission;
        }

        @Override
        public void run() {
            emission.finishEmission();
        }
    }
}
