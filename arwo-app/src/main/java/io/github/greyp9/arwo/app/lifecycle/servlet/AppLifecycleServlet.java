package io.github.greyp9.arwo.app.lifecycle.servlet;

import io.github.greyp9.arwo.app.core.state.AppState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.naming.AppNaming;

import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import java.util.concurrent.ExecutorService;

public class AppLifecycleServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 2426386463726538976L;

    private transient AppState appState;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final String contextPath = getServletContext().getContextPath();
        final Context context = AppNaming.lookupSubcontext(contextPath);
        synchronized (this) {
            this.appState = new AppState(contextPath);
            AppNaming.bind(context, App.Naming.APP_STATE, appState);
            // start threads (ExecutorService lifecycle managed in ExecutorServlet)
            final ExecutorService executor = (ExecutorService)
                    AppNaming.lookup(contextPath, App.Naming.EXECUTOR_SERVICE);
            executor.execute(appState.getCronService());
        }
    }

    @Override
    public final void destroy() {
        final String contextPath = getServletContext().getContextPath();
        final Context context = AppNaming.lookupSubcontext(contextPath);
        synchronized (this) {
            appState.getCronService().stop(getClass().getSimpleName());
            AppNaming.unbind(context, App.Naming.APP_STATE);
            this.appState.shutdown(getClass().getName());
            this.appState = null;
        }
        super.destroy();
    }
}
