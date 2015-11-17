package io.github.greyp9.arwo.core.xed.handler;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.html.CursorHtmlView;
import io.github.greyp9.arwo.core.xml.DocumentU;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;

public class XedHandlerGet {
    private final XedRequest request;

    public XedHandlerGet(final XedRequest request) {
        this.request = request;
    }

    public final HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final String pathInfo = httpRequest.getPathInfo();
        final String query = httpRequest.getQuery();
        final Pather pather = new Pather(pathInfo);
        final boolean isPathInfo = (pathInfo != null);
        final boolean isTrailingSlash = (isPathInfo && (pathInfo.endsWith(Http.Token.SLASH)));
        final boolean isQuery = (query != null);
        final boolean isNoPathInfo = (!isPathInfo);
        final boolean isNoTrailingSlash = (!isTrailingSlash);
        final String baseURI = PathU.toDir(httpRequest.getBaseURI(), Const.CONTEXT_UI);
        if (isNoPathInfo) {
            httpResponse = HttpResponseU.toHttpResponse302(baseURI);
        } else if (isNoTrailingSlash) {
            httpResponse = HttpResponseU.toHttpResponse302(PathU.toDir(httpRequest.getURI()));
        } else if (isQuery) {
            request.getState().applyGet(HttpArguments.toArguments(query));
            httpResponse = HttpResponseU.toHttpResponse302(httpRequest.getURI());
        } else if (Const.CONTEXT_XML.equals(pather.getLeftToken())) {
            httpResponse = doGetXML(httpRequest.getURI(), pather.getRight());
        } else if (Const.CONTEXT_UI.equals(pather.getLeftToken())) {
            httpResponse = doGetUI(httpRequest.getURI(), pather.getRight());
        } else {
            httpResponse = HttpResponseU.toHttpResponse302(baseURI);
        }
        return httpResponse;
    }

    private HttpResponse doGetXML(final String requestURI, final String cursorURI) throws IOException {
        HttpResponse httpResponse = HttpResponseU.toHttpResponse302(PathU.toParent(requestURI));
        final XedCursor cursor = new XedNav(request.getSession().getXed()).find(cursorURI);
        if (cursor != null) {
            final XedCursor cursorConcrete = cursor.getConcrete();
            final byte[] xml = DocumentU.toXml(cursorConcrete.getNode());
            final NameTypeValues headers = new NameTypeValues(
                    new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_XML_UTF8),
                    new NameTypeValue(Http.Header.CONTENT_LENGTH, xml.length));
            httpResponse = new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(xml));
        }
        return httpResponse;
    }

    private HttpResponse doGetUI(final String requestURI, final String cursorURI) throws IOException {
        HttpResponse httpResponse = HttpResponseU.toHttpResponse302(PathU.toParent(requestURI));
        final XedCursor cursor = new XedNav(request.getSession().getXed()).find(cursorURI);
        if (cursor != null) {
            final ServletHttpRequest httpRequest = request.getHttpRequest();
            final String baseURI = PathU.toPath(httpRequest.getBaseURI(), Const.CONTEXT_UI);
            httpResponse = new CursorHtmlView(new XedCursorView(baseURI, cursor), request).doGetHtml();
        }
        return httpResponse;
    }

    private static class Const {
        private static final String CONTEXT_UI = "ui";
        private static final String CONTEXT_XML = "xml";
    }
}
