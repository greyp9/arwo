package io.github.greyp9.arwo.kafka.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.kafka.client.ConsumeKafkaClient;
import io.github.greyp9.arwo.kafka.handler.KafkaHandlerGet;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;
import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class KafkaServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 263712868668644295L;

    private final Logger logger = Logger.getLogger(getClass().getName());

    private transient AppState appState;
    private transient ConsumeKafkaClient consumeKafkaClient;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Context context = AppNaming.lookupSubcontext(getServletContext().getContextPath());
        synchronized (this) {
            this.appState = (AppState) AppNaming.lookup(context, App.Naming.APP_STATE);
            final ExecutorService executor = (ExecutorService) AppNaming.lookup(context, App.Naming.EXECUTOR_SERVICE);
            this.consumeKafkaClient = new ConsumeKafkaClient(executor, ServletU.fromServletConfig(config, "kafka."));
        }
        logger.info("init()");
    }

    @Override
    public final void destroy() {
        logger.info("destroy()");
        synchronized (this) {
            this.consumeKafkaClient.stop("SHUTDOWN");
            this.consumeKafkaClient = null;
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
        final HttpResponse httpResponse = new KafkaHandlerGet(httpRequest, userState, consumeKafkaClient).doGet();
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws IOException {
        logger.info("doPost()");
        final ServletHttpRequest httpRequest = ServletU.read(request);
        // process request
        AppUserState userState;
        synchronized (this) {
            userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        }

        consumeKafkaClient.update(httpRequest, userState.getSubmitID());
        logger.finest(appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate()).toString());
        HttpResponse httpResponse = HttpResponseU.to302(httpRequest.getURI());
        ServletU.write(httpResponse, response);
    }
}
