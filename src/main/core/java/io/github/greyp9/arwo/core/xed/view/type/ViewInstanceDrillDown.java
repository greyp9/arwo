package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

public class ViewInstanceDrillDown extends ViewInstance {
    private final String baseURI;
    private final XedCursor cursorDrillDown;

    public final String getBaseURI() {
        return baseURI;
    }

    public final XedCursor getCursorDrillDown() {
        return cursorDrillDown;
    }

    public ViewInstanceDrillDown(final String baseURI, final XedCursor cursor, final TypeInstance typeInstance) {
        super(cursor, typeInstance);
        this.baseURI = baseURI;
        this.cursorDrillDown = new XedNav(cursor.getXed()).find(typeInstance, cursor);
    }

    @Override
    public final String getValue() {
        return String.format("href(%s)(%s)", cursorDrillDown.getURI(), ">>");  // i18n internal
    }
}
