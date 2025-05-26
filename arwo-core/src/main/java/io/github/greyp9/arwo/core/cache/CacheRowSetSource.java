package io.github.greyp9.arwo.core.cache;

import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;

public final class CacheRowSetSource implements RowSetSource {
    private final ResourceCache cache;
    private final RowSetSource source;
    private final String id;

    public CacheRowSetSource(final ResourceCache cache, final RowSetSource source, final String id) {
        this.cache = cache;
        this.source = source;
        this.id = id;
    }

    @Override
    public String getRowSetId() {
        return source.getRowSetId();
    }

    @Override
    public RowSet getRowSet() throws Exception {
        final RowSet rowSet;
        if (cache.containsRowSet(id)) {
            rowSet = cache.getRowSet(id);
        } else {
            rowSet = source.getRowSet();
            cache.putRowSet(id, rowSet);
        }
        return rowSet;
    }
}
