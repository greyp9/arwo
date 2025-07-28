package io.github.greyp9.arwo.s3.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.s3.handler.S3HandlerGet;

import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Properties;
import java.util.logging.Logger;

public class S3Servlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 263712868668644295L;

    private final Logger logger = Logger.getLogger(getClass().getName());

    private transient AppState appState;
    private transient Properties properties;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Context context = AppNaming.lookupSubcontext(getServletContext().getContextPath());
        synchronized (this) {
            this.appState = (AppState) AppNaming.lookup(context, App.Naming.APP_STATE);
            this.properties = ServletU.fromServletConfig(config, "s3.");
        }
        logger.info("init()");
    }

    @Override
    public final void destroy() {
        logger.info("destroy()");
        synchronized (this) {
            this.properties = null;
            this.appState = null;
        }
    }

    @Override
    protected final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState userState;
        synchronized (this) {
            userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        }
        final HttpResponse httpResponse = new S3HandlerGet(httpRequest, userState, properties).doGet();
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws IOException {
        logger.info("doPost()");
        final HttpResponse httpResponse = HttpResponseU.toError(
                HttpURLConnection.HTTP_NOT_IMPLEMENTED, "HTTP_NOT_IMPLEMENTED");
        ServletU.write(httpResponse, response);
    }
}
