import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;

import java.util.HashMap;
import java.util.Map;

public class Exchange {
    private Map<Integer,Company> companies;
    private ZMQ.Socket push;
    private ZMQ.Socket pull;
    private ZMQ.Socket pub;
    private int companyId;

    public Exchange(Map<Integer,Company> companies ,ZMQ.Socket push, ZMQ.Socket pull, ZMQ.Socket pub){
        this.companies = companies;
        this.push = push;
        this.pull = pull;
        this.pub = pub;
        this.companyId = 0;
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
                            result = exchange.makeBid(sale.getId(), new Bid(sale.getValue(),sale.getRate(),message.getUser().getId()));
                            break;
                        case "company" :
                            result = exchange.addAuction(sale.getId(),sale.getValue(),sale.getRate());
                            break;
                    }
                    break;

                case "emission" :
                    switch (message.getUserType()){
                        case "investor" :
                            result = exchange.makeBuy(sale.getId(), new Buy(sale.getValue(),message.getUser().getId()));
                            break;
                        case "company" :
                            result = exchange.addEmission(sale.getId(), sale.getValue());
                            break;
                    }
                    break;
            }
            Protocol.State state = Protocol.State.newBuilder().setDescription(Boolean.toString(result)).build();
            Protocol.Message response = Protocol.Message.newBuilder().setState(state).setPid(message.getPid()).build();
            push.send(response.toByteArray());


        }


    }

    public static Exchange populate(ZMQ.Socket push, ZMQ.Socket pull, ZMQ.Socket pub){
        Map<Integer,Company> companies = new HashMap<>();
        companies.put(0,new Company(0,"Cesium"));
        companies.put(1,new Company(1,"NECC"));
        companies.put(2,new Company(2,"NEEGIUM"));

        return new Exchange(companies,push,pull,pub);

    }


    private void createCompany(String name){
        int id = getCompanyId();
        this.companies.put(id,new Company(id,name));
    }

    private boolean addAuction(int companyId, Long maxRate, Float rate ){
        return this.companies.get(companyId).addAuction(maxRate, rate, 30000);
    }

    private boolean addEmission(int companyId, Long maxRate){
        return this.companies.get(companyId).addEmission(maxRate, 30000);
    }

    private boolean makeBid(int companyId, Bid bid ){
        return this.companies.get(companyId).makeBid(bid);
    }

    private boolean makeBuy(int companyId, Buy buy){
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
    private synchronized int getCompanyId(){
        return this.companyId++;
    }
}
