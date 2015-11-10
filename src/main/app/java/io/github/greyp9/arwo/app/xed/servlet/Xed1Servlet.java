package io.github.greyp9.arwo.app.xed.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.handler.XedHandlerGet;
import io.github.greyp9.arwo.core.xed.handler.XedHandlerPost;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Locale;
import java.util.TimeZone;

public class Xed1Servlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 781645253051571036L;

    private transient XedUserState state;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        try {
            synchronized (this) {
                final String submitID = Integer.toHexString(hashCode());
                final DateX dateX = new DateX(HttpDateU.Const.DEFAULT, TimeZone.getTimeZone("UTC"));
                final Locus locus = new Locus(Locale.getDefault(), dateX);
                final String contextPath = getServletContext().getContextPath();
                state = new XedUserState(contextPath, submitID, new ViewStates(), new XedClipboard(), locus);
            }
        } catch (IOException e) {
            throw new ServletException(e);
        }
    }

    @Override
    public final void destroy() {
        synchronized (this) {
            this.state = null;
        }
    }

    @Override
    protected final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        XedUserState stateRequest;
        synchronized (this) {
            stateRequest = state;
        }
        final String servletPath = httpRequest.getServletPath();
        final XedRequest xedRequest = new XedRequest(httpRequest, stateRequest.getSession(servletPath), stateRequest);
        final HttpResponse httpResponse = new XedHandlerGet(xedRequest).doGet();
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        XedUserState stateRequest;
        synchronized (this) {
            stateRequest = state;
        }
        final String servletPath = httpRequest.getServletPath();
        final XedRequest xedRequest = new XedRequest(httpRequest, stateRequest.getSession(servletPath), stateRequest);
        final HttpResponse httpResponse = new XedHandlerPost(xedRequest).doPost();
        ServletU.write(httpResponse, response);
    }
}
