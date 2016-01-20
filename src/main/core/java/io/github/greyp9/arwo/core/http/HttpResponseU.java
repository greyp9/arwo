package io.github.greyp9.arwo.core.http;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.ByteArrayInputStream;
import java.net.HttpURLConnection;
import java.util.Date;

public final class HttpResponseU {

    private HttpResponseU() {
    }

    public static HttpResponse to200(final byte[] entity) {
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8),
                new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    public static HttpResponse to200(final MetaFile metaFile) {
        final String lastModified = HttpDateU.toHttpZ(new Date(metaFile.getMetaData().getLastModified()));
        final long length = metaFile.getMetaData().getLength();
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.LAST_MODIFIED, lastModified),
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8),
                new NameTypeValue(Http.Header.CONTENT_LENGTH, length));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, metaFile.getBytes());
    }


    public static HttpResponse to302(final String location) {
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(Http.Header.LOCATION, location));
        return new HttpResponse(HttpURLConnection.HTTP_MOVED_TEMP, headers, null);
    }

    public static HttpResponse to404() {
        return toError(HttpURLConnection.HTTP_NOT_FOUND);
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
