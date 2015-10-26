package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

@SuppressWarnings("PMD.AbstractNaming")
public abstract class ViewInstance {
    private final XedCursor cursor;
    private final TypeInstance typeInstance;

    public final XedCursor getCursor() {
        return cursor;
    }

    public final TypeInstance getTypeInstance() {
        return typeInstance;
    }

    public ViewInstance(final XedCursor cursor, final TypeInstance typeInstance) {
        this.cursor = cursor;
        this.typeInstance = typeInstance;
    }

    public abstract String getValue();
}
