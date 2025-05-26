package io.github.greyp9.arwo.core.table.row;

public interface RowSetSource {
    String getRowSetId();

    RowSet getRowSet() throws Exception;
}
