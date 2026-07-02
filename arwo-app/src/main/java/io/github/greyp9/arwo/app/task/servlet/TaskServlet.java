package io.github.greyp9.arwo.app.task.servlet;

import io.github.greyp9.arwo.app.core.servlet.ServletU;
import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.task.handler.TaskHandlerGet;
import io.github.greyp9.arwo.app.task.handler.TaskHandlerPost;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.gz.HttpResponseGZipU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.task.service.TaskService;
import io.github.greyp9.arwo.core.value.Value;

import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.logging.Logger;

public class TaskServlet extends javax.servlet.http.HttpServlet {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private transient AppState appState;
    private transient TaskService taskService;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Context context = AppNaming.lookupSubcontext(getServletContext().getContextPath());
        this.appState = (AppState) AppNaming.lookup(context, App.Naming.APP_STATE);
        final String taskServiceName = getInitParameter(TaskService.class.getSimpleName());
        this.taskService = Value.as(AppNaming.lookup(
                TaskService.class.getName(), taskServiceName), TaskService.class);
        logger.info(String.format("init():%s:%s", taskServiceName, taskService));
    }

    @Override
    public final void destroy() {
        logger.info(String.format("destroy():%s:%s", taskService.getName(), taskService));
        this.taskService = null;
        this.appState = null;
    }

    @Override
    public final void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // get request context
        final ServletHttpRequest httpRequest = ServletU.read(request);
        final AppUserState userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        final HttpResponse httpResponse = new TaskHandlerGet(httpRequest, userState, taskService).doGet();
        // send response
        final HttpResponse httpResponseGZ = HttpResponseGZipU.toHttpResponseGZip(httpRequest, httpResponse);
        ServletU.write(httpResponseGZ, response);
    }

    @Override
    protected final void doPost(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        // process request
        final ServletHttpRequest httpRequest = ServletU.read(request);
        final AppUserState userState = appState.getUserState(httpRequest.getPrincipal(), httpRequest.getDate());
        final HttpResponse httpResponse = new TaskHandlerPost(httpRequest, userState).doPostSafe();
        // send response
        ServletU.write(httpResponse, response);
    }
}
