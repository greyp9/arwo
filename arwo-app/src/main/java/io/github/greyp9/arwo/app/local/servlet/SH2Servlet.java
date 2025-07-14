package io.github.greyp9.arwo.app.local.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh2.handler.SHHandlerGet;
import io.github.greyp9.arwo.app.local.sh2.handler.SHHandlerPost;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.exec.AppExecutorService;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.naming.AppNaming;

import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class SH2Servlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = -1860523744889746061L;

    private transient AppState appState;
    private transient AppExecutorService executor;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Context context = AppNaming.lookupSubcontext(getServletContext().getContextPath());
        synchronized (this) {
            this.appState = (AppState) AppNaming.lookup(context, App.Naming.APP_STATE);
            this.executor = (AppExecutorService) AppNaming.lookup(context, App.Naming.EXECUTOR);
        }
    }

    @Override
    public final void destroy() {
        synchronized (this) {
            this.executor = null;
            this.appState = null;
        }
    }

    @Override
    protected final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        final HttpResponse httpResponse = new SHHandlerGet(httpRequest, userState).doGet();
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
        AppUserState userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        final HttpResponse httpResponse = new SHHandlerPost(httpRequest, userState, executor).doPostSafe();
        // send response
        ServletU.write(httpResponse, response);
    }
}
