package io.github.greyp9.arwo.core.http;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.ByteArrayInputStream;
import java.net.HttpURLConnection;

public final class HttpResponseU {

    private HttpResponseU() {
    }

    public static HttpResponse to200Text(final String text) {
        final byte[] entity = UTF8Codec.toBytes(text);
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8),
                new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    public static HttpResponse to302(final String location) {
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(Http.Header.LOCATION, location));
        return new HttpResponse(HttpURLConnection.HTTP_MOVED_TEMP, headers, null);
    }

    public static HttpResponse to501() {
        return toError(HttpURLConnection.HTTP_NOT_IMPLEMENTED);
    }

    public static HttpResponse toError(final int statusCode) {
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8));
        final byte[] entity = UTF8Codec.toBytes(Integer.toString(statusCode));
        return new HttpResponse(statusCode, headers, new ByteArrayInputStream(entity));
    }
}
