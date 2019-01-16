package main.java;

public class Client {
    private enum STATE{
        LOGIN,
        REGISTER,
        AUCTION,
        BUY,
        NOTIFICATION,
        MENU
    }

    private enum TYPE{
        COMPANY,
        INVESTOR,
        NON
    }

    private String frontendAddress;
    private STATE state;
    private TYPE type;

    public Client( String,  String frontendAddress){
        this.frontendAddress = frontendAddress;
        this.state = STATE.LOGIN;
        this.type = TYPE.NON;
    }
}

class Writer{
    public Writer(){

    }
}

class Reader{

}
