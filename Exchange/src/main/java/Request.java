import com.google.gson.Gson;

public class Request {
    private int exchangeid;
    private String empresa;
    private Long value;
    private Float rate;

    public Request(int exchangeid, String empresa, Long value, Float rate) {
        this.exchangeid = exchangeid;
        this.empresa = empresa;
        this.value = value;
        rate = rate;
    }

    public Request(int exchangeid, String empresa, Long value) {
        this.exchangeid = exchangeid;
        this.empresa = empresa;
        this.value = value;
        rate = new Float(-1);
    }

    public Request(int exchangeid, String empresa) {
        this.exchangeid = exchangeid;
        this.empresa = empresa;
        this.value = new Long(-1);
        this.rate = new Float(-1);
    }
    public Request(int exchangeid) {
        this.exchangeid = exchangeid;
        this.empresa = "";
        this.value = new Long(-1);
        this.rate = new Float(-1);
    }
}
