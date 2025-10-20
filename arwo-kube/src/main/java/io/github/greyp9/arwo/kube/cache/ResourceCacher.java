package io.github.greyp9.arwo.kube.cache;

import io.github.greyp9.arwo.core.cache.ResourceCache;

public abstract class ResourceCacher<T> {
    private final ResourceCache resourceCache;

    public ResourceCacher(final ResourceCache resourceCache) {
        this.resourceCache = resourceCache;
    }

    public final ResourceCache getResourceCache() {
        return resourceCache;
    }

    abstract void cache(T resource);
}
