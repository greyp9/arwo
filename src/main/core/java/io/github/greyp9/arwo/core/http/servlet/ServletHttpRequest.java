package io.github.greyp9.arwo.core.http.servlet;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.http.HttpRequest;

import java.security.Principal;
import java.util.Date;

public class ServletHttpRequest {
    private final HttpRequest httpRequest;
    private final Date date;
    private final Principal principal;
    private final String contextPath;
    private final String servletPath;
    private final String pathInfo;

    public final HttpRequest getHttpRequest() {
        return httpRequest;
    }

    public final Date getDate() {
        return DateU.copy(date);
    }

    public final Principal getPrincipal() {
        return principal;
    }

    public final String getContextPath() {
        return contextPath;
    }

    public final String getServletPath() {
        return servletPath;
    }

    public final String getPathInfo() {
        return pathInfo;
    }

    public ServletHttpRequest(final HttpRequest httpRequest, final Date date, final Principal principal,
                              final String contextPath, final String servletPath, final String pathInfo) {
        this.httpRequest = httpRequest;
        this.date = DateU.copy(date);
        this.contextPath = contextPath;
        this.servletPath = servletPath;
        this.pathInfo = pathInfo;
        this.principal = principal;
    }

    public final String getHeader(final String name) {
        return httpRequest.getHeader(name);
    }

    public final String getBaseURI() {
        return contextPath + servletPath;
    }

    public final String getURI() {
        return getBaseURI() + pathInfo;
    }

    public final String getQuery() {
        return httpRequest.getQuery();
    }
}
