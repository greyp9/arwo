package io.github.greyp9.arwo.core.http;

import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.ByteArrayInputStream;

public class HttpRequest {
    private final String method;
    private final String resource;
    private final String query;
    private final NameTypeValues headers;
    private final ByteArrayInputStream entity;

    public final String getMethod() {
        return method;
    }

    public final String getResource() {
        return resource;
    }

    public final String getQuery() {
        return query;
    }

    public final NameTypeValues getHeaders() {
        return headers;
    }

    public final ByteArrayInputStream getEntity() {
        return entity;
    }

    public HttpRequest(final String method, final String resource, final String query,
                       final NameTypeValues headers, final ByteArrayInputStream entity) {
        this.method = method;
        this.resource = resource;
        this.query = query;
        this.headers = headers;
        this.entity = entity;
    }

    public final String getHeader(final String name) {
        return headers.getValueIC(name);
    }

    public final String toString() {
        return String.format("[%s][%s][%s][%d]", method, resource, query, headers.size());
    }
}
