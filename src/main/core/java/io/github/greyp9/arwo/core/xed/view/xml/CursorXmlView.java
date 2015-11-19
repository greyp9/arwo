package io.github.greyp9.arwo.core.xed.view.xml;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xml.DocumentU;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;

public class CursorXmlView {
    private final XedCursor cursor;

    public CursorXmlView(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public final HttpResponse doGetXML() throws IOException {
        final XedCursor cursorConcrete = cursor.getConcrete();
        final byte[] xml = DocumentU.toXml(cursorConcrete.getNode());
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_XML_UTF8),
                new NameTypeValue(Http.Header.CONTENT_LENGTH, xml.length));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(xml));
    }
}
