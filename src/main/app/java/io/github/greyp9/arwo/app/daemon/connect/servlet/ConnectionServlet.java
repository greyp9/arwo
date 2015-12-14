package io.github.greyp9.arwo.app.daemon.connect.servlet;

import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.app.daemon.connect.runnable.ConnectionRunnable;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.naming.AppNaming;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import java.util.concurrent.ExecutorService;

public class ConnectionServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 2644952348493257587L;

    private transient ConnectionRunnable monitorRunnable;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        final String contextPath = config.getServletContext().getContextPath();
        final ExecutorService executor = (ExecutorService)
                AppNaming.lookup(contextPath, App.Naming.EXECUTOR_SERVICE);
        final AppState appState = (AppState)
                AppNaming.lookup(contextPath, App.Naming.APP_STATE);
        monitorRunnable = new ConnectionRunnable(appState, appState.getReference());
        executor.execute(monitorRunnable);
    }

    @Override
    public final void destroy() {
        monitorRunnable.getClass();
        super.destroy();
    }
}
