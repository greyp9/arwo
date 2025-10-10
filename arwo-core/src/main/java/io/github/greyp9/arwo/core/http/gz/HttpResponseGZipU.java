package io.github.greyp9.arwo.core.http.gz;

import io.github.greyp9.arwo.core.codec.gz.GZipCodec;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;

import java.io.ByteArrayInputStream;
import java.io.IOException;

public final class HttpResponseGZipU {

    private HttpResponseGZipU() {
    }

    public static HttpResponse toHttpResponseGZip(
            final ServletHttpRequest httpRequest, final HttpResponse httpResponseIn) throws IOException {
        return toHttpResponseGZip(httpRequest.getHttpRequest(), httpResponseIn);
    }

    public static HttpResponse toHttpResponseGZip(
            final HttpRequest httpRequest, final HttpResponse httpResponseIn) throws IOException {
        HttpResponse httpResponse = httpResponseIn;
        final String acceptEncoding = httpRequest.getHeader(Http.Header.ACCEPT_ENCODING);
        final boolean wantGZip = ((acceptEncoding != null) && acceptEncoding.contains(Http.Header.GZIP));
        final String contentType = httpResponse.getHeaders().getValue(Http.Header.CONTENT_TYPE);
        final String contentEncoding = httpResponse.getHeaders().getValue(Http.Header.CONTENT_ENCODING);
        final boolean isGZip = Http.Mime.APP_GZIP.equals(contentType);
/*
        final String contentType = httpResponse.getHeaders().getValue(Http.Header.CONTENT_TYPE);
*/
        final boolean needGZip = ((contentEncoding == null) && (!isGZip) && httpResponse.isEntity());
/*
        java.util.logging.Logger.getLogger(HttpResponseGZipU.class.getName()).info(String.format(
                "RESOURCE=[%s] WANT=[%s] NEED=[%s] ACCEPT_ENCODING=[%s] CONTENT_ENCODING=[%s] CONTENT_TYPE=[%s]",
                httpRequest.getResource(), wantGZip, needGZip, acceptEncoding, contentEncoding, contentType));
*/
        if (wantGZip && needGZip) {
            final byte[] entity = StreamU.read(httpResponse.getEntity());
            final byte[] entityGZip = new GZipCodec().encode(entity);
            final int statusCode = httpResponse.getStatusCode();
            final NameTypeValues headers = NameTypeValuesU.filterOut(httpResponse.getHeaders(),
                    Http.Header.CONTENT_LENGTH, Http.Header.CONTENT_ENCODING);
            headers.add(new NameTypeValue(Http.Header.CONTENT_ENCODING, Http.Header.GZIP));
            headers.add(new NameTypeValue(Http.Header.CONTENT_LENGTH, entityGZip.length));
            httpResponse = new HttpResponse(statusCode, headers, new ByteArrayInputStream(entityGZip));
/*
            java.util.logging.Logger.getLogger(HttpResponseGZipU.class.getName()).info(
                    String.format("TO RESOURCE=[%s] CONTENT_ENCODING=[%s] CONTENT_TYPE=[%s] CONTENT_LENGTH=[%d]->[%d]",
                            httpRequest.getResource(), Http.Header.GZIP, contentType,
                            entity.length, entityGZip.length));
*/
        }
        return httpResponse;
    }
}
