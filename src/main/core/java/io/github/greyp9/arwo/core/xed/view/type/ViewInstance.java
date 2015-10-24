package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

@SuppressWarnings("PMD.AbstractNaming")
public abstract class ViewInstance {
    private final XedCursor cursor;

    public final XedCursor getCursor() {
        return cursor;
    }

    public ViewInstance(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public final TypeInstance getTypeInstance() {
        return cursor.getTypeInstance();
    }

    public abstract String getValue();
}
