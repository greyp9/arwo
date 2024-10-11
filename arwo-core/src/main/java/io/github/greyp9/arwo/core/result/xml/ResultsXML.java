package io.github.greyp9.arwo.core.result.xml;

import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.type.rowset.RowSetResult;
import io.github.greyp9.arwo.core.result.type.text.TextResult;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.util.Date;

// i18nf
public class ResultsXML {
    protected static final String NS_RESULTS = "urn:arwo:xml";

    protected static final String E_NULL = "null";
    protected static final String E_RESULTS = Results.class.getSimpleName();
    protected static final String E_ROW_SET_RESULT = RowSetResult.class.getSimpleName();
    protected static final String E_TEXT_RESULT = TextResult.class.getSimpleName();
    protected static final String E_ROW_SET = RowSet.class.getSimpleName();
    protected static final String E_ROW = Row.class.getSimpleName();
    protected static final String E_META_DATA = RowSetMetaData.class.getSimpleName();
    protected static final String E_COLUMN = ColumnMetaData.class.getSimpleName();
    protected static final String E_OBJECT = Object.class.getSimpleName();
    protected static final String E_STRING = String.class.getSimpleName();
    protected static final String E_INTEGER = Integer.class.getSimpleName();
    protected static final String E_LONG = Long.class.getSimpleName();
    protected static final String E_DATE = Date.class.getSimpleName();
    protected static final String E_TABLE_LINK = TableViewLink.class.getSimpleName();

    protected static final String A_COMMAND = "command";
    protected static final String A_CONTEXT = "context";
    protected static final String A_START = "start";
    protected static final String A_FINISH = "finish";
    protected static final String A_ID = "id";
    protected static final String A_IDENTITY = "identity";
    protected static final String A_LABEL = "label";
    protected static final String A_NAME = "name";
    protected static final String A_TYPE = "type";
    protected static final String A_TEXT = "text";
    protected static final String A_TITLE = "title";
    protected static final String A_HREF = "href";

    protected ResultsXML() {
        // checkstyle: "Utility classes should not have a public or default constructor."
    }
}
