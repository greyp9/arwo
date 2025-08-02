package io.github.greyp9.arwo.core.cache;

import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.table.row.RowSet;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Supplier;

public final class ResourceCache {
    private final String endpoint;
    private final Map<String, RowSet> rowSets;
    private final Map<String, MetaFile> files;
    private final Map<String, Object> objects;

    public String getEndpoint() {
        return endpoint;
    }

    public ResourceCache(final String endpoint) {
        this.endpoint = endpoint;
        this.rowSets = new TreeMap<>();
        this.files = new TreeMap<>();
        this.objects = new TreeMap<>();
    }

    public boolean containsRowSet(final String id) {
        return rowSets.containsKey(id);
    }

    public RowSet getRowSet(final String id) {
        return rowSets.get(id);
    }

    public void putRowSet(final String id, final RowSet rowSet) {
        if (rowSet == null) {
            rowSets.remove(id);
        } else {
            rowSets.put(id, rowSet);
        }
    }

    public Iterator<Map.Entry<String, MetaFile>> getFiles() {
        return files.entrySet().iterator();
    }

    public boolean containsFile(final String id) {
        return files.containsKey(id);
    }

    public MetaFile getFile(final String id) {
        return files.get(id);
    }

    public void putFile(final String id, final MetaFile file) {
        files.put(id, file);
    }

    public synchronized Object getObject(final String id, final Supplier<Object> supplier) {
        final Object o;
        if (supplier == null) {
            o = objects.remove(id);
        } else if (objects.containsKey(id)) {
            o = objects.get(id);
        } else {
            o = supplier.get();
            objects.put(id, o);
        }
        return o;
    }

    public void clear() {
        rowSets.clear();
        files.clear();
        objects.clear();
    }

    public long getSize() throws IOException {
        long size = 0L;
        for (final MetaFile file : files.values()) {
            size += StreamU.read(file.getBytes()).length;
        }
        return size;
    }
}
