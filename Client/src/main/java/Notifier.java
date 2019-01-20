
import org.zeromq.ZMQ;

public class Notifier implements Runnable{
    private ZMQ.Socket sub;
    private State state;

    public Notifier(ZMQ.Socket sub, State state){
        this.state = state;
        this.sub = sub;
    }

    @Override
    public void run() {
        String msg;
        while(true) {
            byte[] b = sub.recv();
            msg = new String(b);
            state.addNotification(msg);
        }
    }
}
