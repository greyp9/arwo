package io.github.greyp9.arwo.core.httpclient.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.gz.GZipCodec;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.logging.Logger;

public class HttpResponseGZipTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    void testTextNoop() throws IOException {
        final NameTypeValues headersIn = NameTypeValuesU.create();
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/hello.txt", null, headersIn, null);
        logger.finest(httpRequest.getResource());

        final byte[] bytesOut = UTF8Codec.toBytes("hello");
        final NameTypeValues headersOut1 = NameTypeValuesU.create(
                Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8,
                Http.Header.CONTENT_LENGTH, Integer.toString(bytesOut.length));
        final HttpResponse httpResponse1 = new HttpResponse(HttpURLConnection.HTTP_OK, headersOut1, bytesOut);

        final HttpResponse httpResponse = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse1);
        final NameTypeValues headersOut = httpResponse.getHeaders();
        Assertions.assertEquals(Http.Mime.TEXT_PLAIN_UTF8, headersOut.getValue(Http.Header.CONTENT_TYPE));
        Assertions.assertEquals(Integer.toString(bytesOut.length), headersOut.getValue(Http.Header.CONTENT_LENGTH));
        Assertions.assertNull(headersOut.getValue(Http.Header.CONTENT_ENCODING));
        Assertions.assertEquals(bytesOut.length, StreamU.read(httpResponse.getEntity()).length);
    }

    @Test
    void testTextTransform() throws IOException {
        final NameTypeValues headersIn = NameTypeValuesU.create(
                Http.Header.ACCEPT_ENCODING, Http.Header.GZIP);
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/hello.txt", null, headersIn, null);
        logger.finest(httpRequest.getResource());

        final byte[] bytesOut = UTF8Codec.toBytes("hello");
        final byte[] bytesOutGZ = new GZipCodec().encode(bytesOut);
        final NameTypeValues headersOut1 = NameTypeValuesU.create(
                Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8,
                Http.Header.CONTENT_LENGTH, Integer.toString(bytesOut.length));
        final HttpResponse httpResponse1 = new HttpResponse(HttpURLConnection.HTTP_OK, headersOut1, bytesOut);

        final HttpResponse httpResponse = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse1);
        final NameTypeValues headersOut = httpResponse.getHeaders();
        Assertions.assertEquals(Http.Mime.TEXT_PLAIN_UTF8, headersOut.getValue(Http.Header.CONTENT_TYPE));
        Assertions.assertEquals(Integer.toString(bytesOutGZ.length), headersOut.getValue(Http.Header.CONTENT_LENGTH));
        Assertions.assertEquals(Http.Header.GZIP, headersOut.getValue(Http.Header.CONTENT_ENCODING));
        Assertions.assertEquals(bytesOutGZ.length, StreamU.read(httpResponse.getEntity()).length);
    }

    @Test
    void testTextPassthroughNoAccept() throws IOException {
        final NameTypeValues headersIn = NameTypeValuesU.create();
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/hello.txt.gz", null, headersIn, null);
        logger.finest(httpRequest.getResource());

        final byte[] bytesOutGZ = new GZipCodec().encode(UTF8Codec.toBytes("hello"));
        final NameTypeValues headersOut1 = NameTypeValuesU.create(
                Http.Header.CONTENT_TYPE, Http.Mime.APP_GZIP,
                Http.Header.CONTENT_LENGTH, Integer.toString(bytesOutGZ.length));
        final HttpResponse httpResponse1 = new HttpResponse(HttpURLConnection.HTTP_OK, headersOut1, bytesOutGZ);

        final HttpResponse httpResponse = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse1);
        final NameTypeValues headersOut = httpResponse.getHeaders();
        Assertions.assertEquals(Http.Mime.APP_GZIP, headersOut.getValue(Http.Header.CONTENT_TYPE));
        Assertions.assertEquals(Integer.toString(bytesOutGZ.length), headersOut.getValue(Http.Header.CONTENT_LENGTH));
        Assertions.assertNull(headersOut.getValue(Http.Header.CONTENT_ENCODING));
        Assertions.assertEquals(bytesOutGZ.length, StreamU.read(httpResponse.getEntity()).length);
    }

    @Test
    void testTextPassthrough() throws IOException {
        final NameTypeValues headersIn = NameTypeValuesU.create(
                Http.Header.ACCEPT_ENCODING, Http.Header.GZIP);
        final HttpRequest httpRequest = new HttpRequest(Http.Method.GET, "/hello.txt.gz", null, headersIn, null);
        logger.finest(httpRequest.getResource());

        final byte[] bytesOutGZ = new GZipCodec().encode(UTF8Codec.toBytes("hello"));
        final NameTypeValues headersOut1 = NameTypeValuesU.create(
                Http.Header.CONTENT_TYPE, Http.Mime.APP_GZIP,
                Http.Header.CONTENT_LENGTH, Integer.toString(bytesOutGZ.length));
        final HttpResponse httpResponse1 = new HttpResponse(HttpURLConnection.HTTP_OK, headersOut1, bytesOutGZ);

        final HttpResponse httpResponse = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse1);
        final NameTypeValues headersOut = httpResponse.getHeaders();
        Assertions.assertEquals(Http.Mime.APP_GZIP, headersOut.getValue(Http.Header.CONTENT_TYPE));
        Assertions.assertEquals(Integer.toString(bytesOutGZ.length), headersOut.getValue(Http.Header.CONTENT_LENGTH));
        Assertions.assertNull(headersOut.getValue(Http.Header.CONTENT_ENCODING));
        Assertions.assertEquals(bytesOutGZ.length, StreamU.read(httpResponse.getEntity()).length);
    }
}
