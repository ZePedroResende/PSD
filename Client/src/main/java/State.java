
import java.util.ArrayDeque;
import Protos.Protocol;
public class State {

    public enum STATE{
        LOGIN,
        REGISTER,
        AUCTION,
        BUY,
        NOTIFICATION,
        MENU
    }

    public enum TYPE{
        COMPANY,
        INVESTOR,
        NON
    }

    private int command;
    private STATE state;
    private TYPE type;
    private int reply;
    private boolean success;
    private String content;
    private Protocol.Message message;
    private ArrayDeque<String> notifications;
    private ArrayDeque<String> sales;


    public State(){
        this.command = 1;
        this.message = null;
        this.state = STATE.LOGIN;
        this.type = TYPE.NON;
        this.reply = 0;
        this.success = false;
        this.content = null;
        this.notifications = new ArrayDeque<>();
        this.sales = new ArrayDeque<>();
    }

    public synchronized int getCommand() {
        return command;
    }

    public synchronized void setCommand(int command) {
        this.command = command;
    }

    public synchronized STATE getState() {
        return state;
    }

    public synchronized void setState(STATE state) {
        this.state = state;
    }

    public synchronized TYPE getType() {
        return type;
    }

    public synchronized String getTypeString() {
        return type.equals(TYPE.COMPANY) ? "company" : "investor";
    }


    public synchronized void setType() {
        this.type = message.getUserType().equals("company") ? TYPE.COMPANY : TYPE.INVESTOR;
    }

    public synchronized int getReply() {
        return reply;
    }

    public synchronized boolean isLoggedIn(){
        return this.type != TYPE.NON;
    }

    public synchronized void setReply(Boolean success, String content, Protocol.Message message) {
        this.success = success;
        this.content = content;
        this.message = message;
        this.reply++;
        notifyAll();
    }

    public synchronized String getResponse()  {
        int responseId = this.reply;

        while(this.reply == responseId){
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        return content;
    }

    public synchronized String getMessages( Boolean sale ){
        ArrayDeque<String> array  = sale ? this.sales : this.notifications;
        return getMessagesQueue(array);
    }

    public synchronized boolean isSuccess() {
        return success;
    }

    public synchronized void setSuccess(boolean success) {
        this.success = success;
    }

    public synchronized String getContent() {
        return content;
    }

    public synchronized void setContent(String content) {
        this.content = content;
    }

    public synchronized int getNumberSales(){
       return this.sales.size();
    }

    public synchronized int getNumberNotifications(){
        return this.notifications.size();
    }

    public synchronized void addSale(String sale){
        this.sales.add(sale);
    }

    public synchronized void addNotification(String notification){
        this.notifications.add(notification);
    }

    public Protocol.Message getMessage() {
        return message;
    }

    public void setMessage(Protocol.Message message) {
        this.message = message;
    }

    synchronized public String getNotifications() {
        return getMessagesQueue(notifications);
    }

    synchronized public String getSales() {
        return getMessagesQueue(sales);
    }

    private String getMessagesQueue(ArrayDeque<String> sales) {
        String msg;
        StringBuilder sb = new StringBuilder();

        while((msg = sales.pollFirst()) != null)
            sb.append(msg).append("\n");

        if (sb.length() > 0)
            sb.deleteCharAt(sb.length() - 1);

        return sb.toString();
    }

}
