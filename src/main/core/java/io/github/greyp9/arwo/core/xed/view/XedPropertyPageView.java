package io.github.greyp9.arwo.core.xed.view;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceFactory;

import java.util.Collection;

public class XedPropertyPageView {
    private final XedCursor cursor;
    private final Collection<ViewInstance> viewInstances;
    private final ActionButtons buttons;

    public final XedCursor getCursor() {
        return cursor;
    }

    public final Collection<ViewInstance> getViewInstances() {
        return viewInstances;
    }

    public final ActionButtons getButtons() {
        return buttons;
    }

    public XedPropertyPageView(final String baseURI, final XedCursor cursor) {
        this(baseURI, cursor, null);
    }

    public XedPropertyPageView(final String baseURI, final XedCursor cursor, final ActionButtons buttons) {
        this.cursor = cursor;
        this.viewInstances = new ViewInstanceFactory(baseURI, cursor).getPageInstances();
        this.buttons = buttons;
    }
}
