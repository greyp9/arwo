package io.github.greyp9.arwo.core.xed.handler;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.html.CursorHtmlView;

import java.io.IOException;

public class XedHandlerGet {
    private final XedSession session;

    public XedHandlerGet(final XedSession session) {
        this.session = session;
    }

    @SuppressWarnings("PMD.ConfusingTernary")
    public final HttpResponse doGet(final ServletHttpRequest httpRequest) throws IOException {
        HttpResponse httpResponse;
        final String pathInfo = httpRequest.getPathInfo();
        final String query = httpRequest.getQuery();
        if (pathInfo == null) {
            httpResponse = HttpResponseU.toHttpResponse302(PathU.toDir(httpRequest.getBaseURI()));
        } else if (!pathInfo.endsWith(Http.Token.SLASH)) {
            httpResponse = HttpResponseU.toHttpResponse302(PathU.toDir(httpRequest.getURI()));
        } else if (query != null) {
            httpResponse = HttpResponseU.toHttpResponse302(httpRequest.getURI());
        } else {
            httpResponse = doGetDocument(httpRequest);
        }
        return httpResponse;
    }

    private HttpResponse doGetDocument(final ServletHttpRequest httpRequest) throws IOException {
        HttpResponse httpResponse;
        final XedCursor cursor = new XedNav(session.getXed()).find(httpRequest.getPathInfo());
        if (cursor == null) {
            httpResponse = HttpResponseU.toHttpResponse302(PathU.toDir(httpRequest.getBaseURI()));
        } else {
            final String baseURI = httpRequest.getBaseURI();
            httpResponse = new CursorHtmlView(httpRequest, new XedCursorView(baseURI, cursor)).doGetHtml();
        }
        return httpResponse;
    }
}
