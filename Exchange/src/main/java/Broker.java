
import org.zeromq.ZMQ;

public class Broker {
    public static void main(String[] args) {
        ZMQ.Context context = ZMQ.context(1);
        ZMQ.Socket push = context.socket(ZMQ.PULL);
        push.bind("tcp://*:" + args[0]);
        ZMQ.Socket pull = context.socket(ZMQ.PUSH);
        pull.bind("tcp://*:" + args[1]);
        ZMQ.proxy(push, pull, null);
    }
}