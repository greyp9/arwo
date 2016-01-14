package io.github.greyp9.arwo.core.result.xml;

import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

public class ResultsReader extends ResultsXML {

    public final Results readFrom(final MetaFile metaFile) throws IOException {
        Results results = null;
        final Document document = DocumentU.toDocument(StreamU.read(metaFile.getBytes()));
        final Element element = document.getDocumentElement();
        final boolean isName = element.getLocalName().equals(E_RESULTS);
        final boolean isNamespace = element.getNamespaceURI().equals(NS_RESULTS);
        if (isName && isNamespace) {
            final String command = ElementU.getAttribute(element, A_COMMAND);
            final Date start = XsdDateU.fromXSDZ(ElementU.getAttribute(element, A_START));
            final Date finish = XsdDateU.fromXSDZ(ElementU.getAttribute(element, A_FINISH));
            results = new Results(command, new Interval(start, finish));
            readFrom(ElementU.getChildren(element), results);
        }
        return results;
    }

    private void readFrom(final Collection<Element> elementsResult, final Results results) {
        for (final Element element : elementsResult) {
            final String name = element.getLocalName();
            if (name.equals(E_TEXT_RESULT)) {
                readTextResult(element, results);
            } else if (name.equals(E_ROW_SET_RESULT)) {
                readRowSetResult(element, results);
            }
        }
    }

    private void readTextResult(final Element elementResult, final Results results) {
        final String id = ElementU.getAttribute(elementResult, A_ID);
        final String type = ElementU.getAttribute(elementResult, A_TYPE);
        final String text = ElementU.getTextContent(elementResult);
        results.add(id, type, text);
    }

    private void readRowSetResult(final Element elementResult, final Results results) {
        final String id = ElementU.getAttribute(elementResult, A_ID);
        final String type = ElementU.getAttribute(elementResult, A_TYPE);
        final Element elementRowSet = ElementU.getChild(elementResult, E_ROW_SET);
        if (elementRowSet != null) {
            results.add(id, type, readRowSet(elementRowSet));
        }
    }

    private RowSet readRowSet(final Element elementRowSet) {
        final Element elementMetaData = ElementU.getChild(elementRowSet, E_META_DATA);
        final RowSetMetaData metaData = (elementMetaData == null) ? null : readRowSetMetaData(elementMetaData);
        final RowSet rowSet = new RowSet(metaData, null, null);
        final Collection<Element> children = ElementU.getChildren(elementRowSet, E_ROW);
        for (final Element child : children) {
            readRow(child, rowSet);
        }
        return rowSet;
    }

    private RowSetMetaData readRowSetMetaData(final Element elementMetaData) {
        final String id = ElementU.getAttribute(elementMetaData, A_ID);
        final Collection<ColumnMetaData> columns = new ArrayList<ColumnMetaData>();
        final Collection<Element> children = ElementU.getChildren(elementMetaData, E_COLUMN);
        for (final Element child : children) {
            columns.add(readColumnMetaData(child));
        }
        final ColumnMetaData[] columnMetaData = columns.toArray(new ColumnMetaData[columns.size()]);
        return new RowSetMetaData(id, columnMetaData);
    }

    private ColumnMetaData readColumnMetaData(final Element child) {
        final String name = ElementU.getAttribute(child, A_NAME);
        final String label = ElementU.getAttribute(child, A_LABEL);
        final int type = TypeU.toIntegerP(ElementU.getAttribute(child, A_TYPE));
        final boolean identity = TypeU.toBooleanP(ElementU.getAttribute(child, A_IDENTITY));
        return new ColumnMetaData(name, label, type, identity);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void readRow(final Element row, final RowSet rowSet) {
        final InsertRow insertRow = new InsertRow(rowSet);
        final Collection<Element> children = ElementU.getChildren(row);
        for (final Element child : children) {
            final String name = child.getLocalName();
            final String text = child.getTextContent();
            if (name.equals(E_NULL)) {
                insertRow.setNextColumn(null);
            } else if (name.equals(E_STRING)) {
                insertRow.setNextColumn(text);
            } else if (name.equals(E_INTEGER)) {
                insertRow.setNextColumn(TypeU.toInteger(text));
            } else if (name.equals(E_LONG)) {
                insertRow.setNextColumn(TypeU.toInteger(text));
            } else if (name.equals(E_DATE)) {
                insertRow.setNextColumn(XsdDateU.fromXSDZ(text));
            } else if (name.equals(E_TABLE_LINK)) {
                final String textLink = ElementU.getAttribute(child, A_TEXT);
                final String title = ElementU.getAttribute(child, A_TITLE);
                final String href = ElementU.getAttribute(child, A_HREF);
                insertRow.setNextColumn(new TableViewLink(textLink, title, href));
            } else {
                insertRow.setNextColumn(null);
            }
        }
        rowSet.add(insertRow.getRow());
    }
}
