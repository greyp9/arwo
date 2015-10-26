package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

public class ViewInstanceDrillDown extends ViewInstance {
    private final XedCursor cursorDrillDown;

    public ViewInstanceDrillDown(final XedCursor cursor, final TypeInstance typeInstance) {
        super(cursor, typeInstance);
        this.cursorDrillDown = new XedNav(cursor.getXed()).find(typeInstance, cursor);
    }

    public final String getValue() {
        return String.format("href(%s)(%s)", cursorDrillDown.getURI(), ">>");
    }
}
