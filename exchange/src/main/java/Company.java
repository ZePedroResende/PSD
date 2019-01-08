import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class Company {
    private Integer id;
    private String name;
    private Map<Integer,Auction> auctions;
    private Map<Integer,Emission> emissions;
    private Integer currentSale;

    public Company(Integer id, String name) {
        this.id = id;
        this.name = name;
        this.auctions = new HashMap<>();
        this.emissions = new HashMap<>();
        this.currentSale = 0;
    }


    public Integer getId() {
        return id;
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

    public Boolean addAuction(Long maxAmount, Integer maxRate, Integer time){
        if()
        this.auctions.put(currentSale++, new Auction(maxAmount, maxRate, this.id, time));
    }

    public Boolean addEmission(Long maxAmount, Integer time){
        this.emissions.put(currentSale, new Emission(this.id, maxAmount, time, ));
        currentSale++;
    }

    public void getRate(){
        Set<Map.Entry<Integer,Sale>> allSales = this.auctions.entrySet();
        allSales.addAll(this.emissions.entrySet());

        if(currentSale)
        this.auctions.entrySet().stream().filter((s)-> s.getValue().ge)
    }


}
