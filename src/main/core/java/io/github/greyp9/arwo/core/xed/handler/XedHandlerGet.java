package io.github.greyp9.arwo.core.xed.handler;

import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.state.XedUserState;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.html.CursorHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.RevisionHtmlView;
import io.github.greyp9.arwo.core.xed.view.xml.CursorXmlView;
import io.github.greyp9.arwo.core.xed.view.xml.SessionXsdView;

import java.io.IOException;
import java.util.Locale;

public class XedHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final XedUserState documentState;
    private final XedEntry entryQ;

    public XedHandlerGet(final ServletHttpRequest httpRequest,
                         final XedUserState documentState, final XedEntry entryQ) {
        this.httpRequest = httpRequest;
        this.documentState = documentState;
        this.entryQ = entryQ;
    }

    public final HttpResponse doGetSafe() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doGet();
        } catch (IOException e) {
            documentState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to500(e.getMessage());
        }
        return httpResponse;
    }

    private HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        final XedSession session = documentState.getSession(httpRequest.getServletPath(), entryQ);
        final XedRequest request = new XedRequest(httpRequest, session, documentState);
        final String pathInfo = httpRequest.getPathInfo();
        final String query = httpRequest.getQuery();
        final Pather pather = new Pather(pathInfo);
        final boolean isPathInfo = (pathInfo != null);
        final boolean isTrailingSlash = (isPathInfo && (pathInfo.endsWith(Http.Token.SLASH)));
        final boolean isQuery = (query != null);
        final boolean isNoPathInfo = (!isPathInfo);
        final boolean isNoTrailingSlash = (!isTrailingSlash);
        final String baseURI = PathU.toDir(httpRequest.getBaseURI(), App.Action.UI);
        if (isNoPathInfo) {
            httpResponse = HttpResponseU.to302(baseURI);
        } else if (isNoTrailingSlash) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getURI()));
        } else if (isQuery) {
            request.getState().applyGet(HttpArguments.toArguments(query));
            httpResponse = HttpResponseU.to302(httpRequest.getURI());
        } else if (App.Action.XML.equals(pather.getLeftToken())) {
            httpResponse = doGetXML(request, pather.getRight());
        } else if (App.Action.XSD.equals(pather.getLeftToken())) {
            httpResponse = new SessionXsdView(request.getSession()).doGetXSD();
        } else if (App.Action.REVISIONS.equals(pather.getLeftToken())) {
            httpResponse = new RevisionHtmlView(request).doGetHtml();
        } else if (App.Action.UI.equals(pather.getLeftToken())) {
            httpResponse = doGetUI(request, pather.getRight());
        } else {
            httpResponse = HttpResponseU.to302(baseURI);
        }
        return httpResponse;
    }

    private HttpResponse doGetXML(final XedRequest request, final String cursorURI) throws IOException {
        final XedCursor cursor = new XedNav(request.getSession().getXed()).find(cursorURI);
        return ((cursor == null) ? HttpResponseU.to302(PathU.toParent(httpRequest.getURI())) :
                new CursorXmlView(cursor).doGetXML());
    }

    private HttpResponse doGetUI(final XedRequest request, final String cursorURI) throws IOException {
        final XedFactory factory = documentState.getFactory();
        final Locale locale = documentState.getLocus().getLocale();
        final XedCursor cursor = new XedNav(request.getSession().getXedUI(factory, locale)).find(cursorURI);
        return ((cursor == null) ? HttpResponseU.to302(PathU.toParent(httpRequest.getURI())) :
                doGetUI(request, cursor));
    }

    private HttpResponse doGetUI(final XedRequest request, final XedCursor cursor) throws IOException {
        final String baseURI = PathU.toPath(httpRequest.getBaseURI(), App.Action.UI);
        return new CursorHtmlView(request, new XedCursorView(baseURI, cursor)).doGetHtml();
    }
}
