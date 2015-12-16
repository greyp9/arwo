package io.github.greyp9.arwo.core.cache;

import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

public class ResourceCache {
    private final Map<String, RowSet> rowSets;
    private final Map<String, MetaFile> files;

    public ResourceCache() {
        this.rowSets = new TreeMap<String, RowSet>();
        this.files = new TreeMap<String, MetaFile>();
    }

    public final boolean containsRowSet(final String id) {
        return rowSets.containsKey(id);
    }

    public final RowSet getRowSet(final String id) {
        return rowSets.get(id);
    }

    public final void putRowSet(final String id, final RowSet rowSet) {
        if (rowSet == null) {
            rowSets.remove(id);
        } else {
            rowSets.put(id, rowSet);
        }
    }

    public final boolean containsFile(final String id) {
        return files.containsKey(id);
    }

    public final MetaFile getFile(final String id) {
        return files.get(id);
    }

    public final void putFile(final String id, final MetaFile file) {
        files.put(id, file);
    }

    public final void clear() {
        rowSets.clear();
        files.clear();
    }

    public final long getSize() throws IOException {
        long size = 0L;
        for (final MetaFile file : files.values()) {
            size += StreamU.read(file.getBytes()).length;
        }
        return size;
    }
}
