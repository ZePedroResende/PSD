
import com.google.protobuf.InvalidProtocolBufferException;
import org.zeromq.ZMQ;
import Protos.Protocol;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;

public class Reader implements Runnable{
    private InputStream in;
    private State state;

    public Reader(Socket socket, State state) throws IOException {
        this.in = socket.getInputStream();
        this.state = state;
    }

    @Override
    public void run() {

        Protocol.Message m;
        while(((m = getMessage()) != null)) {
            updateState(m);

        }

        System.out.println("\nConnection ended by the server.");
        System.exit(1);
    }

    private void updateState(Protocol.Message message){
        Protocol.State state = message.getState();
        String result, description;

        result = state.getResult();
        description = state.getDescription();

        if (result.equals("false"))
            this.state.setReply(false, "> " + description, message );
        else if (result.equals("true"))
            this.state.setReply(true, description, message);
        else
            this.state.addSale(description);
    }

    private Protocol.Message getMessage(){

        Protocol.Message message = null;
        try {
            byte[] bytes = recive();
            message = Protocol.Message.parseFrom(bytes);

        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        }
        return message;
    }

    private byte[] recive(){
        try {
            byte len[] = new byte[4096];
            int count = 0;
            count = in.read(len);
            byte[] temp = new byte[count];
            for (int i = 0; i < count; i++) {
                temp[i] = len[i];
            }
            return temp;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}

