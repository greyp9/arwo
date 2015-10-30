package io.github.greyp9.arwo.app.xed.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xed.handler.XedHandlerGet;
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessionFactory;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Locale;

public class Xed1Servlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 781645253051571036L;

    private transient XedSession session;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        try {
            final File webappRoot = AppFolder.getWebappRoot(getServletContext().getContextPath());
            final File realmFile = new File(webappRoot, "root/realm.xml");
            final String xmlPath = realmFile.getCanonicalPath();
            final String xsdPath = URLCodec.toExternalForm(ResourceU.resolve(App.XSD_REALM));
            final XedEntry entry = new XedEntry("/users", xmlPath, xsdPath, null);
            synchronized (this) {
                this.session = new XedSessionFactory(entry).create(App.QNAME_REALM, Locale.getDefault());
            }
        } catch (IOException e) {
            throw new ServletException(e);
        }
    }

    @Override
    public final void destroy() {
        synchronized (this) {
            this.session = null;
        }
    }

    @Override
    protected final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // get user context
        getClass();
        // process request
        XedSession sessionRequest;
        synchronized (this) {
            sessionRequest = session;
        }
        final HttpResponse httpResponse = new XedHandlerGet(sessionRequest).doGet(httpRequest);
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        response.sendError(HttpURLConnection.HTTP_NOT_IMPLEMENTED);
    }
}
