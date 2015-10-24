package io.github.greyp9.arwo.core.xed.view.text;

import io.github.greyp9.arwo.core.value.Matrix;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import java.util.Collection;

public class PropertyPageTextView {
    private final XedPropertyPageView view;

    public PropertyPageTextView(final XedPropertyPageView view) {
        this.view = view;
    }

    public final String render() {
        final XedCursor cursor = view.getCursor();
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final String pageHeader = view.getItemNameI18n(cursor.getTypeInstance(), null);
        final Collection<ViewInstance> pageInstances = view.getViewInstances();
        final Matrix matrix = new Matrix(pageInstances.size(), 2);
        int row = 0;
        for (final ViewInstance viewInstance : pageInstances) {
            matrix.set(row, 0, view.getItemNameI18n(typeInstance, viewInstance.getTypeInstance()));
            matrix.set(row, 1, viewInstance.getValue());
            ++row;
        }
        return String.format("%s%n%s", pageHeader, matrix.render(" | "));
    }
}
