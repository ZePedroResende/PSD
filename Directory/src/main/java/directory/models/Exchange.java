package directory.models;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.NotEmpty;

public class Exchange {
    @NotEmpty
    private final String name;
    @NotEmpty
    private final String host;
    @NotEmpty
    private final int port;

    public Exchange(String name, String host, int port) {
        this.name = name;
        this.host = host;
        this.port = port;
    }

    @JsonProperty
    public String getName() {
        return name;
    }

    @JsonProperty
    public String getHost() {
        return host;
    }

    @JsonProperty
    public int getPort() {
        return port;
    }
}
