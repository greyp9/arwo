package io.github.greyp9.arwo.core.xed.view;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;

import java.util.ArrayList;
import java.util.Collection;

public class XedCursorView {
    private final XedCursor cursor;

    public final XedCursor getCursor() {
        return cursor;
    }

    public XedCursorView(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public final XedPropertyPageView getPageView() {
        return new XedPropertyPageView(cursor);
    }

    public final XedTableView getTableView() {
        return new XedTableView(cursor);
    }

    public final Object[] getViews() {
        final Collection<Object> views = new ArrayList<Object>();
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final Collection<TypeInstance> pageInstances = new TypeInstanceX(typeInstance).getPageInstances();
        if (!pageInstances.isEmpty()) {
            views.add(new XedPropertyPageView(cursor));
        }
        if (!typeInstance.isSingleton()) {
            views.add(new XedTableView(cursor));
        }
        return views.toArray(new Object[views.size()]);
    }
}
