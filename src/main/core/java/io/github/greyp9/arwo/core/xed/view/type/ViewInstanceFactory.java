package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;

import java.util.ArrayList;
import java.util.Collection;

public class ViewInstanceFactory {
    private final XedCursor cursor;

    public ViewInstanceFactory(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public final Collection<ViewInstance> getPageInstances() {
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(typeInstance).getPageInstances();
        final Collection<ViewInstance> viewInstances = new ArrayList<ViewInstance>();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            viewInstances.add(toViewInstancePage(typeInstanceIt));
        }
        return viewInstances;
    }

    public final Collection<ViewInstance> getTableInstances() {
        final TypeInstance typeInstance = cursor.getTypeInstance();
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(typeInstance).getTableInstances();
        final Collection<ViewInstance> viewInstances = new ArrayList<ViewInstance>();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            viewInstances.add(toViewInstanceTable(typeInstanceIt));
        }
        return viewInstances;
    }

    @SuppressWarnings("PMD.ConfusingTernary")
    private ViewInstance toViewInstancePage(final TypeInstance typeInstance) {
        ViewInstance viewInstance = null;
        final DataType dataType = typeInstance.getDataType();
        final boolean isMulti = (typeInstance.getMaxOccurs() > 1);
        if (dataType != null) {
            if (!dataType.getInstances().isEmpty()) {  // is drill down
                final XedCursor cursorType = new XedNav(cursor.getXed()).find(typeInstance.getName(), cursor);
                viewInstance = new ViewInstanceDrillDown(cursorType);
            } else if (!dataType.getRestrictions().getEnumValues().isEmpty()) {
                final XedCursor cursorChild = new XedNav(cursor.getXed()).findChild(typeInstance.getName(), cursor);
                viewInstance = new ViewInstanceEnum(cursorChild);
            }
        }
        if (isMulti) {
            final XedCursor cursorType = new XedNav(cursor.getXed()).find(typeInstance.getName(), cursor);
            viewInstance = new ViewInstanceDrillDown(cursorType);
        }
        if (viewInstance == null) {
            final XedCursor cursorChild = new XedNav(cursor.getXed()).findChild(typeInstance.getName(), cursor);
            viewInstance = new ViewInstanceText(cursorChild);
        }
        return viewInstance;
    }

    private ViewInstance toViewInstanceTable(final TypeInstance typeInstance) {
        return toViewInstancePage(typeInstance);
    }
}
