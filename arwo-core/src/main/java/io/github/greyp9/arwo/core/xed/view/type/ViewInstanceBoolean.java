package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

public class ViewInstanceBoolean extends ViewInstance {

    public ViewInstanceBoolean(final XedCursor cursor, final TypeInstance typeInstance) {
        super(cursor, typeInstance);
    }

    public final String getValue() {
        return getCursor().getValue(getTypeInstance());
    }
}
