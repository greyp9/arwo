package io.github.greyp9.arwo.core.result.type;

public class Result {
    private final String id;
    private final String type;

    public final String getID() {
        return id;
    }

    public final String getType() {
        return type;
    }

    public Result(final String id, final String type) {
        this.id = id;
        this.type = type;
    }
}
