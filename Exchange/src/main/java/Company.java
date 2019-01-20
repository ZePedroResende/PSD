
import org.zeromq.ZMQ;

import java.util.*;
import java.util.stream.Collectors;

public class Company {
    private String name;
    private Map<Integer,Auction> auctions;
    private Map<Integer,Emission> emissions;
    private Integer currentSale;

    public Company( String name) {
        this.name = name;
        this.auctions = new HashMap<>();
        this.emissions = new HashMap<>();
        this.currentSale = 0;
    }

    public String getName() {
        return name;
    }

    public Map<Integer, Auction> getAuctions() {
        return auctions;
    }

    public Map<Integer, Emission> getEmissions() {
        return emissions;
    }

    public Integer getCurrentSale() {
        return currentSale;
    }

    public Boolean addAuction(Long maxAmount, Float maxRate, Integer time, ZMQ.Socket push){
        Boolean result = false;
        if(isAuctionAvailable()){
            this.auctions.put(currentSale++, new Auction(maxAmount, maxRate, this.name, this.currentSale, time, push ));
            result = true;
        }
        return result;
    }

    public Boolean addEmission(Long maxAmount, Integer time, ZMQ.Socket push){
        boolean result = false;

        if(isEmissionAvailable()){
            Float rate = getRate();
            if( rate < 0) return false;
            this.emissions.put(currentSale++, new Emission(this.name, this.currentSale, maxAmount, time, rate, push));
            result = true;
        }

        return  result;
    }

    public boolean isSaleAvailable(){
        return  isAuctionAvailable() && isEmissionAvailable();
    }

    public Float getRate(){
        Float rate = new Float(-1);

        List<Map.Entry<Integer,Sale>> listSale = getSales();

        for( Map.Entry<Integer,Sale> l : listSale){
            Sale sale = l.getValue();
            if(sale.getClass().equals(Auction.class) ){
                if(sale.isSucess()) {
                    return ((Auction) sale).getMinimalRate();
                }
            } else {
                if(sale.isSucess()) {
                    return ((Emission) sale).getRate();
                } else {
                    return new Float( ((Emission) sale).getRate() * 1.1);
                }
            }
        }

        return  rate;
    }

    public boolean makeBid(Bid bid){
        boolean value = false;
        if(isAuctionAvailable()){
            value = this.auctions.get(currentSale - 1).addBid(bid);
        }
        return value;
    }

    public boolean makeBuy(Buy buy){
        boolean value = false;
        if(isEmissionAvailable()){
            value = this.emissions.get(currentSale - 1).addBuy(buy);
        }
        return value;
    }

    private boolean isEmissionAvailable(){
        List<Map.Entry<Integer,Sale>> listSale = getSales();
        if(listSale.size() == 0) return false;
        Sale sale = listSale.get(0).getValue();
        return  !sale.isActive() && (sale.getClass().equals(Auction.class) && sale.isSucess() ) ;
    }

    private boolean isAuctionAvailable(){
        List<Map.Entry<Integer,Sale>> listSale = getSales();
        List<Auction> listAuction=  auctions.values().stream().filter(x -> x.isActive()).collect(Collectors.toList());
        return listAuction.size() == 0;
    }

    private List<Map.Entry<Integer,Sale>> getSales(){
        Map<Integer,Sale> sales = new HashMap<>();
        sales.putAll(auctions);
        sales.putAll(emissions);

        List<Map.Entry<Integer,Sale>> listSale =  sales.entrySet().stream().collect(Collectors.toList());
        Collections.reverse(listSale);
        return listSale;
    }



}
