package io.github.greyp9.arwo.app.core.servlet;

import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.Principal;
import java.util.Date;
import java.util.Enumeration;

public final class ServletU {

    private ServletU() {
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private static NameTypeValues toHeaders(final HttpServletRequest request) {
        final NameTypeValues headers = new NameTypeValues();
        final Enumeration<?> names = request.getHeaderNames();
        while (names.hasMoreElements()) {
            final String name = (String) names.nextElement();
            final Enumeration<?> values = request.getHeaders(name);
            while (values.hasMoreElements()) {
                final String value = (String) values.nextElement();
                headers.add(new NameTypeValue(name, value));
            }
        }
        return headers;
    }

    private static HttpRequest toHttpRequest(final HttpServletRequest request) throws IOException {
        final String method = request.getMethod();
        final String resource = request.getRequestURI();
        final String query = request.getQueryString();
        final NameTypeValues headers = toHeaders(request);
        final byte[] entity = StreamU.read(request.getInputStream());
        return new HttpRequest(method, resource, query, headers, new ByteArrayInputStream(entity));
    }

    public static ServletHttpRequest read(final HttpServletRequest request) throws IOException {
        final HttpRequest httpRequest = toHttpRequest(request);
        final Date date = new Date();
        final Principal principal = request.getUserPrincipal();
        final String contextPath = request.getContextPath();
        final String servletPath = request.getServletPath();
        final String pathInfo = request.getPathInfo();
        return new ServletHttpRequest(httpRequest, date, principal, contextPath, servletPath, pathInfo);
    }

    public static void write(final HttpResponse httpResponse, final HttpServletResponse response) throws IOException {
        response.setStatus(httpResponse.getStatusCode());
        for (final NameTypeValue nameValue : httpResponse.getHeaders()) {
            response.addHeader(nameValue.getName(), nameValue.getValueS());
        }
        if (httpResponse.isEntity()) {
            response.getOutputStream().write(StreamU.read(httpResponse.getEntity()));
        }
    }
}
