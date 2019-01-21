import org.zeromq.ZMQ;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.Socket;
import java.net.URL;
import java.util.NoSuchElementException;
import java.util.Scanner;
import Protos.Protocol;
public class Writer implements Runnable{
    private OutputStream os;
    private State state;
    private ZMQ.Socket sub;
    private String username;
    private Scanner input;
    private Menu menu;
    private String[] sessionMenu;
    private String[] initialMenu;

    public Writer(Socket socket, State state, ZMQ.Socket sub) throws IOException {
        this.os = socket.getOutputStream();
        this.state = state;
        this.sub = sub;
        this.menu = new Menu();
        setUpMenus();

    }

    public void run() {
        int option;

        while((option = showMenu()) != -1) {
            state.setCommand(option);
            try {
                runCommand(option);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        System.out.println("\nConnection ended!");
        System.exit(0);
    }

    private int showMenu() {
        int option = 0;

        try {
            if (!state.isLoggedIn())
                option = menu.show(initialMenu);
            else {
                sessionMenu[0] = "1) Read " + "(" + state.getNumberNotifications() + ") notifications";
                sessionMenu[1] = "2) Check " + "(" + state.getNumberSales() + ") sales" ;
                option = menu.show(sessionMenu) + 2;
            }
        } catch (NoSuchElementException e) {
            return -1;
        }

        return option;
    }


    private void runCommand(int option) throws IOException {
        switch(option) {
            case 1: signup();
                break;
            case 2: login();
                break;
            case 3: readNotifications();
                break;
            case 4: readSales();
                break;
            case 5: subscribeCompany();
                break;
            case 6: unsubscribeCompany();
                break;
            case 7:
                if (state.getType().equals(State.TYPE.COMPANY)) startAuction();
                else buyAuction();
                break;
            case 8:
                if (state.getType().equals(State.TYPE.COMPANY)) startEmission();
                else buyEmission();
                break;
            case 9: try {listCompanies();} catch(Exception e) {System.out.println("Error receiving info");}
                break;
            case 10: try {companyInfo();} catch(Exception e) {System.out.println("Error receiving info");}
                break;
        }
    }

    private void signup() throws IOException {
        String username = menu.readString("Username: ");
        String password = menu.readString("Password: ");
        Boolean company = menu.readBoolean("Company ? (true/false): ");
        String userType = company ? "company" : "investor";
        Protocol.User c = Protocol.User.newBuilder().setUsername(username).setPassword(password).build();
        Protocol.Message req = Protocol.Message.newBuilder().setType("REGISTER").setUserType(userType).setUser(c).build();
        byte[] result = req.toByteArray();
        os.write(result);

        String response = state.getResponse();
        System.out.println("RESPONSE: " +response);
        if (state.isSuccess()) {

            req = Protocol.Message.newBuilder().setType("LOGIN").setUser(c).build();
            result = req.toByteArray();
            os.write(result);

            response = state.getResponse();

            if(state.isSuccess()) {
                this.state.setType();
                this.username = username;
            }
        }

        menu.printResponse(response);
    }


    private void login() throws IOException {
        String username = menu.readString("Username: ");
        String password = menu.readString("Password: ");

        Protocol.User c = Protocol.User.newBuilder().setUsername(username).setPassword(password).build();
        Protocol.Message req = Protocol.Message.newBuilder().setType("LOGIN").setUser(c).build();
        byte[] result = req.toByteArray();
        os.write(result);
        String response = state.getResponse();

        if(state.isSuccess()) {

            state.setType();
            this.username = username;
        }else {
            menu.printResponse(response);
        }
    }

    private void readNotifications() {
        int amountNotifications;
        String notifications;

        synchronized (state) {
            amountNotifications = state.getNumberNotifications();
            notifications = state.getNotifications();
        }

        if (amountNotifications == 0)
            notifications = "> Still no notifications to present!\n";

        menu.printResponse(notifications);
    }

    private void readSales() {
        int amountTransactions;
        String transactions;

        synchronized (state) {
            amountTransactions = state.getNumberSales();
            transactions = state.getSales();
        }

        if (amountTransactions == 0)
            transactions = "> Still no transactions happened!\n";

        menu.printResponse(transactions);
    }

    private void subscribeCompany() {
        String channel = "";
        String tipo = menu.readString("Type of sale to subscribe: ");
        channel = tipo;
        String company = menu.readString("Company to subscribe: ");
        if(company != ""){
            channel = channel + "-" + tipo;
        }

        sub.subscribe(channel.getBytes());
        System.out.println("Sales from " + company + " subscribed.\n");
    }

    private void unsubscribeCompany() {
        String channel = "";
        String tipo = menu.readString("Type of sale to unsubscribe: ");
        channel = tipo;
        String company = menu.readString("Company to unsubscribe: ");
        if(company != ""){
            channel = channel +"-"+ tipo;
        }

        sub.unsubscribe(channel.getBytes());
        System.out.println("Sales from type" + tipo + " " + company + " unsubscribed.\n");
    }

    private void buyEmission() throws IOException {
        String company = menu.readString("Company: ");
        int price = menu.readInt("Ammount : ");
        long priceLong = new Long(price);
        Protocol.User c = Protocol.User.newBuilder().setUsername(username).build();
        Protocol.Sale o = Protocol.Sale.newBuilder().setName(company).setValue(priceLong).build();
        Protocol.Message m = Protocol.Message.newBuilder().setType("emission").setUser(c).setSale(o).setUserType(state.getTypeString()).build();
        byte[] result = m.toByteArray();
        os.write(result);
    }

    private void buyAuction() throws IOException {
        String company = menu.readString("Company: ");
        int price = menu.readInt("Ammount: ");
        float rate = menu.readFloat("Rate: ");

        long priceLong = new Long(price);
        Protocol.User c = Protocol.User.newBuilder().setUsername(username).build();
        Protocol.Sale o = Protocol.Sale.newBuilder().setName(company).setValue(priceLong).setRate(rate).build();
        Protocol.Message m = Protocol.Message.newBuilder().setType("auction").setUser(c).setSale(o).setUserType(state.getTypeString()).build();
        byte[] result = m.toByteArray();
        os.write(result);
    }

    private void startEmission() throws IOException {
        String company = this.username;
        int price = menu.readInt("Ammount : ");
        long priceLong = new Long(price);
        Protocol.User c = Protocol.User.newBuilder().setUsername(username).build();
        Protocol.Sale o = Protocol.Sale.newBuilder().setName(company).setValue(priceLong).build();
        Protocol.Message m = Protocol.Message.newBuilder().setType("emission").setUser(c).setSale(o).setUserType(state.getTypeString()).build();
        byte[] result = m.toByteArray();
        os.write(result);
    }

    private void startAuction() throws IOException {
        String company = this.username;
        int price = menu.readInt("Ammount: ");
        float rate = menu.readFloat(" MAx Rate: ");

        long priceLong = new Long(price);
        Protocol.User c = Protocol.User.newBuilder().setUsername(username).build();
        Protocol.Sale o = Protocol.Sale.newBuilder().setName(company).setValue(priceLong).setRate(rate).build();
        Protocol.Message m = Protocol.Message.newBuilder().setType("auction").setUser(c).setSale(o).setUserType(state.getTypeString()).build();
        byte[] result = m.toByteArray();
        os.write(result);
    }

    private void getRequest(String path) throws IOException {
        URL url = new URL(path);
        HttpURLConnection con = (HttpURLConnection) url.openConnection();
        con.setRequestMethod("GET");

        int response = con.getResponseCode();

        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
        String inputLine;
        StringBuffer reply = new StringBuffer();

        while ((inputLine = in.readLine()) != null) {
            reply.append(inputLine);
        }
        in.close();
        System.out.println(reply.toString());
    }

    private void listCompanies() throws Exception {
        String path = "http://localhost:8080/companies";
        getRequest(path);
    }


    private void companyInfo() throws Exception {
        String company = menu.readString("Company: ");
        String path = "http://localhost:8080/company/" + company;
        getRequest(path);
    }

    private void setUpMenus() {
        initialMenu = new String[2];
        sessionMenu = new String[8];

        initialMenu[0] = "1) Register";
        initialMenu[1] = "2) Login";

        sessionMenu[2] = "3) Subscribe company";
        sessionMenu[3] = "4) Unsubscribe company";
        sessionMenu[4] = "5) auction";
        sessionMenu[5] = "6) emission";
        sessionMenu[6] = "7) List companies";
        sessionMenu[7] = "8) Get company info";
    }

}
