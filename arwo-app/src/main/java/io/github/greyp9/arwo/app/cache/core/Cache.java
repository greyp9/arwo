package io.github.greyp9.arwo.app.cache.core;

public final class Cache {

    private Cache() {
    }

    public static final String CONTEXT_METAFILES = "f";  // CacheServlet context
    public static final String CONTEXT_ROWSETS = "r";  // CacheServlet context

    public static final String TABLE_ID_ROWSETS = "cacheRowSets";  // inventory/A
    public static final String TABLE_ID_METAFILES = "cacheMetaFiles";  // inventory/B
    public static final String TABLE_ID_OBJECTS = "cacheObjects";  // inventory/C
}
