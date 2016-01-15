package io.github.greyp9.arwo.core.xed.view.text;

import io.github.greyp9.arwo.core.value.Matrix;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedTableView;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.util.Collection;

public class TableTextView {
    private final XedTableView view;
    private final XsdBundle bundle;

    public TableTextView(final XedTableView view) {
        this.view = view;
        this.bundle = view.getCursor().getXed().getXsdBundle();
    }

    public final String render() {
        final XedCursor cursor = view.getCursor();
        final TypeInstance instanceTable = cursor.getTypeInstance();
        final XedCursor cursorTable = cursor.getParentConcrete();
        final Element elementTable = cursorTable.getElement();
        final String tableHeader = bundle.getLabel(instanceTable);
        final Collection<ViewInstance> viewInstances = view.getViewInstances();
        final Collection<Element> childrenRows = ElementU.getChildren(
                elementTable, cursor.getTypeInstance().getName());
        final Matrix matrix = new Matrix(1 + childrenRows.size(), viewInstances.size());
        int row = 0;
        int column = -1;
        for (final ViewInstance viewInstance : viewInstances) {
            final String columnName = bundle.getLabel(instanceTable, viewInstance.getTypeInstance());
            matrix.set(row, ++column, columnName);
        }
        final XedNav nav = new XedNav(cursor.getXed());
        for (final Element elementRow : childrenRows) {
            final XedCursor cursorRow = nav.find(elementRow, cursorTable);
            ++row;
            column = -1;
            for (final ViewInstance viewInstance : view.getViewInstances()) {
                final TypeInstance columnInstance = viewInstance.getTypeInstance();
                final String value = cursorRow.getValue(columnInstance);
                matrix.set(row, ++column, value);
            }
        }
        return String.format("%s%n%s", tableHeader, matrix.render(" | "));  // i18n
    }
}
