package io.github.greyp9.arwo.core.xed.table;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.table.cell.Duration;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.filter.Filters;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sorts;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xsd.core.XsdTypeU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.util.Collection;

public class XedRowSetFactory {
    private final String baseURI;
    private final String submitID;
    private final TypeInstance typeInstance;
    private final RowSetMetaData metaData;
    private final Sorts sorts;
    private final Filters filters;

    public XedRowSetFactory(final String baseURI, final String submitID, final TypeInstance typeInstance,
                            final RowSetMetaData metaData, final Sorts sorts, final Filters filters) {
        this.baseURI = baseURI;
        this.submitID = submitID;
        this.typeInstance = typeInstance;
        this.metaData = metaData;
        this.sorts = sorts;
        this.filters = filters;
    }

    public final RowSet create(final XedCursor cursor, final Collection<Element> children, final Element selected) {
        submitID.getClass();

        final XedNav nav = new XedNav(cursor.getXed());
        final RowSet rowSet = new RowSet(metaData, sorts, filters);
        for (final Element child : children) {
            final XedCursor cursorRow = nav.find(child, cursor);
            iterateCursor(cursorRow, selected, rowSet);
        }
        return rowSet;
    }

    private void iterateCursor(final XedCursor cursorRow, final Element selected, final RowSet rowSet) {
        if ((cursorRow.getTypeInstance().equals(typeInstance)) && (cursorRow.getNode() != null)) {
            final boolean isSelected = Value.equal(cursorRow.getElement(), selected);
            iterateRow(rowSet.getRows(), cursorRow, rowSet, isSelected);
        }
    }

    private void iterateRow(final int ordinal, final XedCursor cursor, final RowSet rowSet, final boolean isSelected) {
        int i = -1;
        final Row row = new Row(ordinal, isSelected, metaData.size());
        if (baseURI != null) {  // uncommented to handle display of metrics
            addRowLink(row, ++i, cursor);
        }
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(cursor.getTypeInstance()).getTableInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            iterateColumn(row, ++i, cursor, typeInstanceIt, cursor.getValue(typeInstanceIt));
        }
        rowSet.add(row, true);
    }

    private void iterateColumn(
            final Row row, final int i, final XedCursor cursor, final TypeInstance typeInstanceC, final String value) {
        final DataType dataType = typeInstanceC.getDataType();
        final QName qname = ((dataType == null) ? null : dataType.getQName());
        if (XsdTypeU.Const.BOOLEAN.equals(qname)) {
            row.setColumn(i, TypeU.toBoolean(value));
        } else if (XsdTypeU.Const.INTEGER.equals(qname)) {
            row.setColumn(i, TypeU.toInteger(value));
        } else if (XsdTypeU.Const.UNSIGNED_INT.equals(qname)) {
            row.setColumn(i, TypeU.toInteger(value));
        } else if (XsdTypeU.Const.DATE_TIME.equals(qname)) {
            row.setColumn(i, XsdDateU.fromXSDZ(value));
        } else if (XsdTypeU.Const.DURATION.equals(qname)) {
            row.setColumn(i, new Duration(DurationU.toMillis(value)));
        } else if (typeInstanceC instanceof ChoiceTypeInstance) {
            row.setColumn(i, getChoiceText(value, cursor, (ChoiceTypeInstance) typeInstanceC));
        } else {
            row.setColumn(i, value);
        }
    }

    private void addRowLink(final Row row, final int i, final XedCursor cursor) {
        final String resource = baseURI + cursor.getURI();
        row.setColumn(i, new TableViewLink("...", null, resource));
    }

    private String getChoiceText(final String value, final XedCursor cursor, final ChoiceTypeInstance choiceInstance) {
        final XedNav nav = new XedNav(cursor.getXed());
        final StringBuilder buffer = new StringBuilder();
        final Collection<TypeInstance> typeInstances = choiceInstance.getTypeInstances().getTypeInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            if (typeInstanceIt.getName().equals(value)) {
                final Collection<Element> children = cursor.getChildren(typeInstanceIt);
                for (final Element child : children) {
                    final XedCursor cursorIt = nav.find(child, cursor);
                    buffer.append(cursorIt.getValue(typeInstanceIt));
                }
            }
        }
        return buffer.toString();
    }
}