package io.github.greyp9.arwo.core.http;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;

import java.io.ByteArrayInputStream;
import java.net.HttpURLConnection;
import java.util.Date;

public final class HttpResponseU {

    private HttpResponseU() {
    }

/*
    public static HttpResponse to200(final byte[] entity) {
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8),
                new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }
*/

    public static HttpResponse to200(final MetaFile metaFile) {
        final String lastModified = HttpDateU.toHttpZ(new Date(metaFile.getMetaData().getLastModified()));
        final String contentType = Value.defaultOnEmpty(metaFile.getContentType(), Http.Mime.TEXT_PLAIN_UTF8);
        final long length = metaFile.getMetaData().getLength();
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.LAST_MODIFIED, lastModified),
                new NameTypeValue(Http.Header.CONTENT_TYPE, contentType),
                new NameTypeValue(Http.Header.CONTENT_LENGTH, length));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, metaFile.getBytes());
    }


    public static HttpResponse to302(final String location) {
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(Http.Header.LOCATION, location));
        return new HttpResponse(HttpURLConnection.HTTP_MOVED_TEMP, headers, (byte[]) null);
    }

    public static HttpResponse to404() {
        return toError(HttpURLConnection.HTTP_NOT_FOUND, null);
    }

    public static HttpResponse to500(final String message) {
        return toError(HttpURLConnection.HTTP_INTERNAL_ERROR, message);
    }

    public static HttpResponse toError(final int statusCode, final String message) {
        final String text = Integer.toString(statusCode) + Http.Token.CRLF + ((message == null) ? "" : message);
        final byte[] entity = UTF8Codec.toBytes(text);
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8),
                new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length));
        return new HttpResponse(statusCode, headers, new ByteArrayInputStream(entity));
    }
}
