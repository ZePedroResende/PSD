import com.sun.org.apache.xpath.internal.operations.Bool;

import java.util.*;

public class Emission implements Sale{
    private Integer companyId;
    private Float rate;
    private List<Buy> buys;
    private Long amount;
    private Boolean active;
    private Boolean sucess;

    public Emission(Integer companyId, Long amount, Integer time, Float rate) {
        this.companyId = companyId;
        this.buys = new ArrayList<>();
        this.amount = amount;
        this.active = true;
        this.sucess = false;
        new Timer().schedule(new Finisher(this),time);
    }

    public Integer getCompanyId() {
        return companyId;
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

    public synchronized Map.Entry<Boolean,List<Buy>> getResults() throws InterruptedException {
        while(active){
            wait();
        }

        long value = this.buys.stream()
                .mapToLong(buy -> buy.getOffer())
                .reduce(0,(x,y)-> x + y);

        this.sucess = value >= amount;

        return new AbstractMap .SimpleEntry<>(sucess,buys);
    }

    public synchronized void finishEmission(){
        active = false;
        notifyAll();
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
