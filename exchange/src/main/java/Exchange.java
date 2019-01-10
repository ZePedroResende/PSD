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

    private synchronized int getCompanyId(){
        return this.companyId++;
    }
}
