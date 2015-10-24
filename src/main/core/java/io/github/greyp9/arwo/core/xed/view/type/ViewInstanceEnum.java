package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;

public class ViewInstanceEnum extends ViewInstance {

    public ViewInstanceEnum(final XedCursor cursor) {
        super(cursor);
    }

    public final String getValue() {
        return getCursor().getValue();
    }
}
