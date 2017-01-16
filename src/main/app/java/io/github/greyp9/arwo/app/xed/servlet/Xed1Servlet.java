package io.github.greyp9.arwo.app.xed.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.xed.core.XedU;
import io.github.greyp9.arwo.core.xed.handler.XedHandlerGet;
import io.github.greyp9.arwo.core.xed.handler.XedHandlerPost;
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class Xed1Servlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 781645253051571036L;

    private transient AppState appState;
    private transient XedEntry xedEntry;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Context context = AppNaming.lookupSubcontext(getServletContext().getContextPath());
        synchronized (this) {
            this.appState = (AppState) AppNaming.lookup(context, App.Naming.APP_STATE);
            final String title = getInitParameter(XedU.Entry.TITLE);  // optional
            final String qname = getInitParameter(XedU.Entry.QNAME);
            final String xmlPath = getInitParameter(XedU.Entry.XML);
            final String xsdPath = getInitParameter(XedU.Entry.XSD);
            final String xsltPath = getInitParameter(XedU.Entry.XSLT);  // optional
            final String trigger = getInitParameter(XedU.Entry.TRIGGER);  // optional
            this.xedEntry = new XedEntry(title, null, qname, xmlPath, xsdPath, xsltPath, trigger);
        }
    }

    @Override
    public final void destroy() {
        synchronized (this) {
            this.appState = null;
        }
        super.destroy();
    }

    @Override
    protected final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState userState;
        XedEntry entry;
        synchronized (this) {
            userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
            entry = xedEntry;
        }
        final XedUserState documentState = userState.getDocumentState();
        final HttpResponse httpResponse = new XedHandlerGet(httpRequest, documentState, entry).doGetSafe();
        // send response
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState userState;
        synchronized (this) {
            userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        }
        final XedUserState documentState = userState.getDocumentState();
        final HttpResponse httpResponse = new XedHandlerPost(httpRequest, documentState).doPostSafe();
        // send response
        ServletU.write(httpResponse, response);
    }
}
