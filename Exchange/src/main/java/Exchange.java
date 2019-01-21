
import com.google.gson.Gson;
import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import Protos.Protocol;
public class Exchange {
    private Map<String ,Company> companies;
    private ZMQ.Socket push;
    private ZMQ.Socket pull;
    private ZMQ.Socket pub;
    private String port;
    private int id;

    public Exchange(Map<String,Company> companies ,ZMQ.Socket push, ZMQ.Socket pull, ZMQ.Socket pub, int id, String port ){
        this.companies = companies;
        this.push = push;
        this.pull = pull;
        this.pub = pub;
        this.id = id;
        this.port = port;
        this.directoryExchangeCreate();
    }


    public static void main(String[] args){
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket push = context.socket(ZMQ.PUSH);
        push.connect("tcp://localhost:" + args[0]);

        ZMQ.Socket pull = context.socket(ZMQ.PULL);
        pull.bind("tcp://*:" + args[1]);

        ZMQ.Socket pub = context.socket(ZMQ.PUB);
        pub.connect("tcp://localhost:" + args[2]);

        Exchange exchange = populate(push, pull, pub, Integer.parseInt(args[3]), args[1] );

        while(true){

            Protocol.Message message = exchange.read();
            Protocol.Sale sale = message.getSale();
            Boolean result = false;

            switch (message.getType()){

                case "auction" :
                    switch (message.getUserType()){
                        case "investor" :
                            result = exchange.makeBid(sale.getName(), new Bid(sale.getValue(),sale.getRate(),message.getUser().getUsername()));
                            break;
                        case "company" :
                            result = exchange.addAuction(sale.getName(),sale.getValue(),sale.getRate());
                            break;
                    }
                    break;

                case "emission" :
                    switch (message.getUserType()){
                        case "investor" :
                            result = exchange.makeBuy(sale.getName(), new Buy(sale.getValue(),message.getUser().getUsername()));
                            break;
                        case "company" :
                            result = exchange.addEmission(sale.getName(), sale.getValue());
                            break;
                    }
                    break;
            }
            Protocol.State state = Protocol.State.newBuilder().setResult(Boolean.toString(result)).setDescription(Boolean.toString(result)).build();
            Protocol.User user = Protocol.User.newBuilder().setUsername(message.getUser().getUsername()).build();
            Protocol.Message response = Protocol.Message.newBuilder().setState(state).setUser(user).build();
            push.send(response.toByteArray());


        }


    }

    public static Exchange populate(ZMQ.Socket push, ZMQ.Socket pull, ZMQ.Socket pub, int id, String port){
        Map<String,Company> companies = new HashMap<>();
        if(id == 0){
            companies.put("Cesium",new Company("Cesium"));
            companies.put("NECC",new Company("NECC"));
            companies.put("NEEGIUM",new Company("NEEGIUM"));
        }
        if(id == 1){
            companies.put("Cesium1",new Company("Cesium1"));
            companies.put("NECC1",new Company("NECC1"));
            companies.put("NEEGIUM1",new Company("NEEGIUM1"));
        }

        return new Exchange(companies,push,pull,pub, id, port);

    }


    private void createCompany(String name){
        directoryCompanyCreate(name);
        this.companies.put(name ,new Company(name));
    }

    private boolean addAuction(String companyId, Long maxRate, Float rate ){
        if (!this.companies.containsKey(companyId)){
           createCompany(companyId);
        }
        this.pub.send("auction-"+companyId+" add Auction: Value: "+ maxRate + " Rate:" + rate);
        return this.companies.get(companyId).addAuction(maxRate, rate, 30000, this.push);
    }

    private boolean addEmission(String companyId, Long maxRate){
        if (!this.companies.containsKey(companyId)){
            return false;
        }
        this.pub.send("auction-"+companyId+" add Auction: Value: "+ maxRate );
        return this.companies.get(companyId).addEmission(maxRate, 30000, this.push);
    }

    private boolean makeBid(String companyId, Bid bid ){
        boolean b = this.companies.get(companyId).makeBid(bid);
        if(b){
            synchronized (this.pub){
                this.pub.send("auction-"+companyId+" with bid:"+bid.toString());
            }
        }
        return b;
    }

    private boolean makeBuy(String companyId, Buy buy){
        boolean b = this.companies.get(companyId).makeBuy(buy);
        if(b){
            synchronized (this.pub){
                this.pub.send("emission-"+companyId+" with buy:"+buy.toString());
            }
        }
        return b;
    }

    private void directoryExchangeCreate(){
        Request r = new Request(this.id);
        String json = new Gson().toJson(r);
        sendPostRequest("http://localhost:8080/exchanges?name="+this.id+"&host=localhost&port="+this.port, "");
    }

    private void directoryCompanyCreate(String name){
        Request r = new Request(this.id, name);
        String json = new Gson().toJson(r);
        sendPostRequest("http://localhost:8080/company", json);
    }

    private void directoryAuctionCreate(String company, Long maxRate, Float rate){

        Request r = new Request(this.id, company, maxRate, rate);
        String json = new Gson().toJson(r);
        sendPostRequest("http://localhost:8080/company/"+company+"/auction", json);
    }

    private void directoryEmissionCreate(String company, Long maxRate, Float rate){

        Request r = new Request(this.id, company, maxRate, rate);
        String json = new Gson().toJson(r);
        sendPostRequest("http://localhost:8080/company/" + company + "/emission", json);
    }

    private void sendPostRequest(String urlString, String json){
        URL url = null;
        try {
            url = new URL(urlString);
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }

        HttpURLConnection con = null;
        try {
            con = (HttpURLConnection) url.openConnection();
        } catch (IOException e) {
            e.printStackTrace();
        }
        try {
            con.setRequestMethod("POST");
        } catch (ProtocolException e) {
            e.printStackTrace();
        }
        con.setRequestProperty("Content-Type","application/json");
        con.setDoOutput(true);
        DataOutputStream wr = null;
        try {
            wr = new DataOutputStream(con.getOutputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
        try {
            wr.writeBytes(json);
            wr.flush();
            wr.close();

            int response = con.getResponseCode();
            System.out.println(response);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private Protocol.Message read(){

        Protocol.Message message ;
        byte[] packet = pull.recv();
        try {

            message = Protocol.Message.parseFrom(packet);
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
            message = null;
        }

        return message;
    }
}
