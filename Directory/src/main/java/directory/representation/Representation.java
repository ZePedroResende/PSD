package directory.representation;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Representation<T> {
    final private long code;
    final private T data;

    public Representation(long code, T data) {
        this.code = code;
        this.data = data;
    }

    @JsonProperty
    public long getCode() {
        return code;
    }

    @JsonProperty
    public T getData() {
        return data;
    }
}