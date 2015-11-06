package io.github.greyp9.arwo.core.xed.view;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceFactory;

import java.util.Collection;

public class XedPropertyPageView {
    private final XedCursor cursor;
    private final Collection<ViewInstance> viewInstances;

    public final XedCursor getCursor() {
        return cursor;
    }

    public final Collection<ViewInstance> getViewInstances() {
        return viewInstances;
    }

    public XedPropertyPageView(final String baseURI, final XedCursor cursor) {
        this.cursor = cursor;
        this.viewInstances = new ViewInstanceFactory(baseURI, cursor).getPageInstances();
    }
}
