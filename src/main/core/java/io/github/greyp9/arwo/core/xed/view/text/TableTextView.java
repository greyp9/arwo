package io.github.greyp9.arwo.core.xed.view.text;

import io.github.greyp9.arwo.core.value.Matrix;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedTableView;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Collection;

public class TableTextView {
    private final XedTableView view;

    public TableTextView(final XedTableView view) {
        this.view = view;
    }

    public final String render() {
        final XedCursor cursor = view.getCursor();
        final XedCursor parentConcrete = cursor.getParentConcrete();
        final String tableHeader = view.getItemNameI18n(cursor.getTypeInstance(), null);
        final Collection<ViewInstance> viewInstances = view.getViewInstances();
        final Collection<Element> children = ElementU.getChildren(
                parentConcrete.getElement(), cursor.getTypeInstance().getName());
        final Matrix matrix = new Matrix(1 + children.size(), viewInstances.size());
        final Collection<String> itemNamesI18n = view.getItemNamesI18n();
        int row = 0;
        int column = -1;
        for (final String itemName : itemNamesI18n) {
            matrix.set(row, ++column, itemName);
        }
        final XedNav nav = new XedNav(cursor.getXed());
        for (final Element child : children) {
            final XedCursor cursorConcrete = nav.find(child, cursor.getParentConcrete());
            ++row;
            column = -1;
            for (final ViewInstance tableInstance : view.getViewInstances()) {
                final String nameTI = tableInstance.getTypeInstance().getName();
                final XedCursor cursorTI = nav.findChild(nameTI, cursorConcrete);
                final String value = cursorTI.getValue();
                matrix.set(row, ++column, value);
            }
        }
        return String.format("%s%n%s", tableHeader, matrix.render(" | "));
    }
}
