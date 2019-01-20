
import org.zeromq.ZMQ;

public class NotificationBroker {
    public static void main(String[] args) {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket pubs = context.socket(ZMQ.XSUB);
        ZMQ.Socket subs = context.socket(ZMQ.XPUB);
        System.out.println(pubs.bind("tcp://*:"+args[0]));
        System.out.println(subs.bind("tcp://*:"+args[1]));
        ZMQ.proxy(pubs, subs, null);
    }
}
