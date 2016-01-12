package io.github.greyp9.arwo.core.result.type;

public class Result {
    private final String id;
    private final String type;

    public String getID() {
        return id;
    }

    public String getType() {
        return type;
    }

    public Result(String id, String type) {
        this.id = id;
        this.type = type;
    }
}
