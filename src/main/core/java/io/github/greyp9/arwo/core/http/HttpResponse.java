package io.github.greyp9.arwo.core.http;

import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;

import java.io.ByteArrayInputStream;

public class HttpResponse {
    private final int statusCode;
    private final NameTypeValues headers;
    private final ByteArrayInputStream entity;

    public HttpResponse(final int statusCode, final NameTypeValues headers, final ByteArrayInputStream entity) {
        this.statusCode = statusCode;
        this.headers = Value.defaultOnNull(headers, new NameTypeValues());
        this.entity = entity;
    }

    public final int getStatusCode() {
        return statusCode;
    }

    public final NameTypeValues getHeaders() {
        return headers;
    }

    public final boolean isEntity() {
        return ((entity != null) && (entity.available() > 0));
    }

    public final ByteArrayInputStream getEntity() {
        return entity;
    }

    public final String toString() {
        return String.format("[%d][%s][%d]", statusCode, headers, (isEntity() ? entity.available() : null));
    }
}
