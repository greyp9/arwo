package io.github.greyp9.arwo.core.xed.view.text;

import io.github.greyp9.arwo.core.value.Matrix;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import java.util.Collection;

public class PropertyPageTextView {
    private final XedPropertyPageView view;
    private final XsdBundle bundle;

    public PropertyPageTextView(final XedPropertyPageView view) {
        this.view = view;
        this.bundle = view.getCursor().getXed().getXsdBundle();
    }

    public final String render() {
        final XedCursor cursor = view.getCursor();
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final String pageHeader = bundle.getLabel(typeInstance);
        final Collection<ViewInstance> pageInstances = view.getViewInstances();
        final Matrix matrix = new Matrix(pageInstances.size(), 2);
        int row = 0;
        for (final ViewInstance viewInstance : pageInstances) {
            final TypeInstance typeInstanceIt = viewInstance.getTypeInstance();
            matrix.set(row, 0, bundle.getLabel(typeInstance, typeInstanceIt));
            matrix.set(row, 1, viewInstance.getValue());
            ++row;
        }
        return String.format("%s%n%s", pageHeader, matrix.render(" | "));
    }
}
