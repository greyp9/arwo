package io.github.greyp9.arwo.core.table.test;

import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.table.filter.Filter;
import io.github.greyp9.arwo.core.table.filter.Filters;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.sort.Sort;
import io.github.greyp9.arwo.core.table.sort.Sorts;
import junit.framework.TestCase;
import org.junit.Assert;

import java.io.File;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Iterator;

public class TableTest extends TestCase {

    public void testTableLoadUserHome() throws Exception {
        final File folderHome = FileU.toFile(SystemU.userHome());
        final File[] files = FileU.listFiles(folderHome);
        Assert.assertTrue(files.length > 0);
    }

    public void testLoad() throws Exception {
        final RowSet rowSet = createRowSet(null, null);
        Assert.assertEquals(3, rowSet.getRows());
        final Row rowFirst = rowSet.iterator().next();
        Assert.assertEquals("a", rowFirst.getString(0));
        Assert.assertEquals(Integer.valueOf(1), rowFirst.getInteger(1));
    }

    public void testSort() throws Exception {
        final Sort sort = new Sort("letter", false);
        final Sorts sorts = new Sorts(sort);
        final RowSet rowSet = createRowSet(sorts, null);
        Assert.assertEquals(3, rowSet.getRows());
        final Row row1 = rowSet.iterator().next();
        Assert.assertEquals("c", row1.getString(0));
        Assert.assertEquals(Integer.valueOf(3), row1.getInteger(1));
    }

    public void testFilter() throws Exception {
        final Filter filter = new Filter(0, "letter", Filter.Operator.EQ, "b");
        final Filters filters = new Filters(filter);
        final RowSet rowSet = createRowSet(null, filters);
        Assert.assertEquals(1, rowSet.getRows());
        final Row row1 = rowSet.iterator().next();
        Assert.assertEquals("b", row1.getString(0));
        Assert.assertEquals(Integer.valueOf(2), row1.getInteger(1));
    }

    public void testSortAndFilter() throws Exception {
        final Sort sort = new Sort("letter", false);
        final Sorts sorts = new Sorts(sort);
        final Filter filter = new Filter(0, "letter", Filter.Operator.LEQ, "b");
        final Filters filters = new Filters(filter);
        final RowSet rowSet = createRowSet(sorts, filters);
        Assert.assertEquals(2, rowSet.getRows());
        final Iterator<Row> iterator = rowSet.iterator();
        final Row row1 = iterator.next();
        Assert.assertEquals("b", row1.getString(0));
        Assert.assertEquals(Integer.valueOf(2), row1.getInteger(1));
        final Row row2 = iterator.next();
        Assert.assertEquals("a", row2.getString(0));
        Assert.assertEquals(Integer.valueOf(1), row2.getInteger(1));
    }

    private static RowSet createRowSet(Sorts sorts, Filters filters) throws Exception {
        final ArrayList<ColumnMetaData> columns = new ArrayList<ColumnMetaData>();
        columns.add(new ColumnMetaData("letter", Types.VARCHAR));
        columns.add(new ColumnMetaData("number", Types.VARCHAR));
        final RowSetMetaData metaData = new RowSetMetaData("id", columns.toArray(new ColumnMetaData[columns.size()]));
        final RowSet rowSet = new RowSet(metaData, sorts, filters);
        insertRow(rowSet, "a", 1);
        insertRow(rowSet, "b", 2);
        insertRow(rowSet, "c", 3);
        return rowSet;
    }

    private static void insertRow(RowSet rowSet, String letter, int number) {
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(letter);
        insertRow.setNextColumn(number);
        rowSet.add(insertRow.getRow());
    }
}
