import org.zeromq.ZMQ;

import java.io.IOException;
import java.net.Socket;

public class Client {

    private String frontendAddress;
    private ZMQ.Socket sub;
    private State state;
    private Thread t1;
    private Thread t2;
    private Thread t3;


    public Client(String frontendAddress, int frontendPort) throws IOException {
        this.frontendAddress = frontendAddress;
        ZMQ.Context context = ZMQ.context(1);
        sub = context.socket(ZMQ.SUB);
        sub.connect(frontendAddress);
        Socket socketJava = new Socket(frontendAddress, frontendPort);
        this.state = new State();
        this.t1 = new Thread(new Writer(socketJava, state, sub));
        this.t2 = new Thread(new Reader(socketJava, state));
        this.t3 = new Thread(new Notifier(sub, state));
        t1.start();
        t2.start();
        t3.start();
        try {
            t1.join();
            t2.join();
            t3.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }


    public static void main(String[] args) throws Exception, IOException {
        new Client(args[0],Integer.parseInt(args[1]));
    }
}
