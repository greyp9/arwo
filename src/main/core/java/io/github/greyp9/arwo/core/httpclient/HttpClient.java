package io.github.greyp9.arwo.core.httpclient;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Map;

public class HttpClient {
    private final Proxy proxy;

    public HttpClient() {
        this(null);
    }

    public HttpClient(final Proxy proxy) {
        this.proxy = proxy;
    }

    protected Proxy getProxy() {
        return proxy;
    }

    protected URLConnection getConnection(final URL url) throws IOException {
        return ((proxy == null) ? url.openConnection() : url.openConnection(proxy));
    }

    public HttpResponse doRequest(
            final URL url, final HttpRequest httpRequest) throws IOException {
        final URL urlRequest = toURL(url, httpRequest);
        return doRequest((HttpURLConnection) getConnection(urlRequest), httpRequest);
    }

    public final HttpResponse doRequest(
            final HttpURLConnection connection, final HttpRequest httpRequest) throws IOException {
        // configure connection
        connection.setRequestMethod(httpRequest.getMethod());
        connection.setDoInput(true);
        connection.setDoOutput(httpRequest.getEntity() != null);
        connection.setInstanceFollowRedirects(false);
        // send
        setHeaders(connection, httpRequest);
        connection.connect();
        setEntity(connection, httpRequest);
        // receive
        return receive(connection);
    }

    protected static URL toURL(final URL url, final HttpRequest httpRequest) throws MalformedURLException {
        final String resource = httpRequest.getResource();
        final String query = httpRequest.getQuery();
        final String file = Value.join(Http.Token.QUERY, resource, query);
        return new URL(url.getProtocol(), url.getHost(), url.getPort(), file);
    }

    private static void setHeaders(final HttpURLConnection connection, final HttpRequest httpRequest) {
        final NameTypeValues headers = httpRequest.getHeaders();
        for (String name : headers.getNames()) {
            final String value = Value.joinCollection(Http.Token.COMMA, headers.getValues(name));
            connection.setRequestProperty(name, value);
        }
    }

    private static void setEntity(
            final HttpURLConnection connection, final HttpRequest httpRequest) throws IOException {
        final ByteArrayInputStream entity = httpRequest.getEntity();
        if (entity != null) {
            StreamU.writeFully(entity, connection.getOutputStream());
        }
    }

    private static HttpResponse receive(final HttpURLConnection connection) throws IOException {
        final int statusCode = connection.getResponseCode();
        final NameTypeValues headers = new NameTypeValues();
        final Map<String, List<String>> headerFields = connection.getHeaderFields();
        for (Map.Entry<String, List<String>> entry : headerFields.entrySet()) {
            final String name = entry.getKey();
            for (String value : entry.getValue()) {
                headers.add(name, value);
            }
        }
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        try {
            StreamU.writeFully(connection.getInputStream(), os);
        } catch (IOException e) {
            StreamU.writeFully(connection.getErrorStream(), os);
        }
        return new HttpResponse(statusCode, headers, os.toByteArray());
    }
}
