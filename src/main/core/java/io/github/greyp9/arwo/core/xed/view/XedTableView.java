package io.github.greyp9.arwo.core.xed.view;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceFactory;

import java.util.Collection;

public class XedTableView {
    private final String baseURI;
    private final XedCursor cursor;
    private final Collection<ViewInstance> viewInstances;

    public final String getBaseURI() {
        return baseURI;
    }

    public final XedCursor getCursor() {
        return cursor;
    }

    public final Collection<ViewInstance> getViewInstances() {
        return viewInstances;
    }

    public XedTableView(final String baseURI, final XedCursor cursor) {
        this.baseURI = baseURI;
        this.cursor = cursor;
        this.viewInstances = new ViewInstanceFactory(baseURI, cursor).getTableInstances();
    }
}
