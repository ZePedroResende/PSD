package main.java;

import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.util.HashMap;
import java.util.Map;

public class Exchange {
    private Map<String ,Company> companies;
    private ZMQ.Socket push;
    private ZMQ.Socket pull;
    private ZMQ.Socket pub;

    public Exchange(Map<String,Company> companies ,ZMQ.Socket push, ZMQ.Socket pull, ZMQ.Socket pub){
        this.companies = companies;
        this.push = push;
        this.pull = pull;
        this.pub = pub;
    }


    public static void main(String[] args){
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket push = context.socket(ZMQ.PUSH);
        push.connect("tcp://localhost:" + args[0]);

        ZMQ.Socket pull = context.socket(ZMQ.PULL);
        pull.bind("tcp://*:" + args[1]);

        ZMQ.Socket pub = context.socket(ZMQ.PUB);
        pub.connect("tcp://localhost:" + args[2]);

        Exchange exchange = populate(push, pull, pub);

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
            Protocol.State state = Protocol.State.newBuilder().setDescription(Boolean.toString(result)).build();
            Protocol.User user = Protocol.User.newBuilder().setUsername(message.getUser().getUsername()).build();
            Protocol.Message response = Protocol.Message.newBuilder().setState(state).setUser(user).build();
            push.send(response.toByteArray());


        }


    }

    public static Exchange populate(ZMQ.Socket push, ZMQ.Socket pull, ZMQ.Socket pub){
        Map<String,Company> companies = new HashMap<>();
        companies.put("Cesium",new Company("Cesium"));
        companies.put("NECC",new Company("NECC"));
        companies.put("NEEGIUM",new Company("NEEGIUM"));

        return new Exchange(companies,push,pull,pub);

    }


    private void createCompany(String name){
        this.companies.put(name ,new Company(name));
    }

    private boolean addAuction(String companyId, Long maxRate, Float rate ){
        return this.companies.get(companyId).addAuction(maxRate, rate, 30000, this.push);
    }

    private boolean addEmission(String companyId, Long maxRate){
        return this.companies.get(companyId).addEmission(maxRate, 30000, this.push);
    }

    private boolean makeBid(String companyId, Bid bid ){
        return this.companies.get(companyId).makeBid(bid);
    }

    private boolean makeBuy(String companyId, Buy buy){
       return this.companies.get(companyId).makeBuy(buy);
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
