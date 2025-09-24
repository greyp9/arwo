package io.github.greyp9.arwo.core.http.gz;

import io.github.greyp9.arwo.core.codec.gz.GZipCodec;
import io.github.greyp9.arwo.core.http.Http;
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
        HttpResponse httpResponse = httpResponseIn;
        final String acceptEncoding = httpRequest.getHeader(Http.Header.ACCEPT_ENCODING);
        final boolean wantGZip = ((acceptEncoding != null) && acceptEncoding.contains(Http.Header.GZIP));
        final String responseEncoding = httpResponse.getHeaders().getValue(Http.Header.CONTENT_ENCODING);
        final boolean needGZip = ((responseEncoding == null) && httpResponse.isEntity());
        if (wantGZip && needGZip) {
            final byte[] entity = StreamU.read(httpResponse.getEntity());
            final byte[] entityGZip = new GZipCodec().encode(entity);
            final int statusCode = httpResponse.getStatusCode();
            final NameTypeValues headers = NameTypeValuesU.filterOut(
                    httpResponse.getHeaders(), Http.Header.CONTENT_ENCODING, Http.Header.CONTENT_LENGTH);
            headers.add(new NameTypeValue(Http.Header.CONTENT_ENCODING, Http.Header.GZIP));
            headers.add(new NameTypeValue(Http.Header.CONTENT_LENGTH, entityGZip.length));
            httpResponse = new HttpResponse(statusCode, headers, new ByteArrayInputStream(entityGZip));
        }
        return httpResponse;
    }
}
