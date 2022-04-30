package io.github.greyp9.arwo.core.expr;

public abstract class Node {
    public abstract String render();

    private Object result;

    public final Object getResult() {
        return result;
    }

    public final void setResult(final Object result) {
        this.result = result;
    }
}
