package io.github.greyp9.arwo.kube.cache;

import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.openapi.models.V1Secret;

public final class V1SecretCacher extends ResourceCacher<V1Secret> {
    private final String endpoint;

    public V1SecretCacher(final ResourceCache resourceCache, final String endpoint) {
        super(resourceCache);
        this.endpoint = endpoint;
    }

    @Override
    public void cache(final V1Secret v1Secret) {
        final V1ObjectMeta metadata = v1Secret.getMetadata();
        String namespace = (metadata == null) ? null : metadata.getNamespace();
        String name = (metadata == null) ? null : metadata.getName();
        final String key = String.format("%s/%s/%s", endpoint, namespace, name);
        getResourceCache().putObject(key, v1Secret);
    }
}
