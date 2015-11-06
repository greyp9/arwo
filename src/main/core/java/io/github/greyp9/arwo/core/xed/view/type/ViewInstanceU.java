package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;

import java.util.ArrayList;
import java.util.Collection;

public final class ViewInstanceU {

    private ViewInstanceU() {
    }

    public static Collection<String> getItemNames(final Collection<ViewInstance> viewInstances) {
        final Collection<String> names = new ArrayList<String>();
        for (final ViewInstance viewInstance : viewInstances) {
            names.add(viewInstance.getTypeInstance().getName());
        }
        return names;
    }

    public static Collection<String> getItemNamesI18n(
            final XedCursor cursor, final Collection<ViewInstance> viewInstances) {
        final Collection<String> names = new ArrayList<String>();
        for (final ViewInstance viewInstance : viewInstances) {
            final XsdBundle xsdBundle = viewInstance.getCursor().getXed().getXsdBundle();
            names.add(xsdBundle.getLabel(cursor.getTypeInstance(), viewInstance.getTypeInstance()));
        }
        return names;
    }
}
