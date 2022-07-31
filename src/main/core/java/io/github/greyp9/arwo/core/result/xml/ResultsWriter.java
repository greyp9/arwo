package io.github.greyp9.arwo.core.result.xml;

import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.type.Result;
import io.github.greyp9.arwo.core.result.type.rowset.RowSetResult;
import io.github.greyp9.arwo.core.result.type.text.TextResult;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.Iterator;

public class ResultsWriter extends ResultsXML {

    public final void writeTo(final File file, final Results results) throws IOException {
        if (file != null) {
            final Document document = DocumentU.createDocument(E_RESULTS, NS_RESULTS);
            writeTo(document, results);
            FileU.ensureFolders(file.getParentFile());
            StreamU.write(file, DocumentU.toXml(document));
        }
    }

    private void writeTo(final Document document, final Results results) {
        final Element elementResults = document.getDocumentElement();
        ElementU.setAttributes(elementResults, NTV.create(
                A_CONTEXT, results.getContext(),
                A_COMMAND, results.getCommand(),
                A_START, XsdDateU.toXSDZMillis(results.getInterval().getDateStart()),
                A_FINISH, XsdDateU.toXSDZMillis(results.getInterval().getDateFinish())));
        for (final Result result : results.getResults()) {
            writeTo(elementResults, result);
        }
    }

    private void writeTo(final Element elementResults, final Result result) {
        if (result instanceof TextResult) {
            writeTo(elementResults, (TextResult) result);
        } else if (result instanceof RowSetResult) {
            writeTo(elementResults, (RowSetResult) result);
        }
    }

    private void writeTo(final Element elementResults, final TextResult result) {
        final NameTypeValues ntv = NTV.create(A_ID, result.getID(), A_TYPE, result.getType());
        ElementU.addElement(elementResults, E_TEXT_RESULT, result.getText(), ntv);
    }

    private void writeTo(final Element elementResults, final RowSetResult result) {
        final NameTypeValues ntv = NTV.create(A_ID, result.getID(), A_TYPE, result.getType());
        final Element elementResult = ElementU.addElement(elementResults, E_ROW_SET_RESULT, null, ntv);
        writeTo(elementResult, result.getRowSet());
    }

    private void writeTo(final Element elementResult, final RowSet rowSet) {
        final Element elementRowSet = ElementU.addElement(elementResult, E_ROW_SET);
        writeTo(elementRowSet, rowSet.getMetaData());
        final Iterator<Row> iterator = rowSet.iterator();
        while (iterator.hasNext()) {
            writeTo(elementRowSet, rowSet.getMetaData(), iterator.next());
        }
    }

    private void writeTo(final Element elementRowSet, final RowSetMetaData metaData) {
        final NameTypeValues ntv = NTV.create(A_ID, metaData.getID());
        final Element elementMetaData = ElementU.addElement(elementRowSet, E_META_DATA, null, ntv);
        final int size = metaData.size();
        for (int i = 0; (i < size); ++i) {
            writeTo(elementMetaData, metaData.getColumnMetaData(i));
        }
    }

    private void writeTo(final Element elementMetaData, final ColumnMetaData column) {
        final NameTypeValues ntv = NTV.create(A_NAME, column.getName(), A_LABEL, column.getLabel(),
                A_TYPE, TypeU.toString(column.getType()), A_IDENTITY, TypeU.toString(column.isIdentity()));
        ElementU.addElement(elementMetaData, E_COLUMN, null, ntv);
    }

    private void writeTo(final Element elementRowSet, final RowSetMetaData metaData, final Row row) {
        final Element elementRow = ElementU.addElement(elementRowSet, E_ROW);
        final int size = metaData.size();
        for (int i = 0; (i < size); ++i) {
            writeTo(elementRow, row.getColumn(i));
        }
    }

    private void writeTo(final Element elementRow, final Object column) {
        if (column == null) {
            ElementU.addElement(elementRow, E_NULL);
        } else if (column instanceof String) {
            ElementU.addElement(elementRow, E_STRING, TypeU.toString(column));
        } else if (column instanceof Integer) {
            ElementU.addElement(elementRow, E_INTEGER, TypeU.toString((Integer) column));
        } else if (column instanceof Long) {
            ElementU.addElement(elementRow, E_LONG, TypeU.toString((Long) column));
        } else if (column instanceof Date) {
            ElementU.addElement(elementRow, E_DATE, XsdDateU.toXSDZ((Date) column));
        } else if (column instanceof TableViewLink) {
            final TableViewLink link = (TableViewLink) column;
            ElementU.addElement(elementRow, E_TABLE_LINK, null, NTV.create(
                    A_TEXT, link.getText(), A_TITLE, link.getTitle(), A_HREF, link.getHref()));
        } else {
            ElementU.addElement(elementRow, E_OBJECT, column.toString());
        }
    }
}
