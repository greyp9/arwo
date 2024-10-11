package io.github.greyp9.arwo.core.table.baseline;

public class BaselineValue {
    private final Object valueNew;
    private final Object valueOld;

    public final Object getNew() {
        return valueNew;
    }

    public final Object getOld() {
        return valueOld;
    }

    public BaselineValue(final Object valueNew, final Object valueOld) {
        this.valueNew = valueNew;
        this.valueOld = valueOld;
    }
}
