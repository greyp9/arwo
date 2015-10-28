package io.github.greyp9.arwo.app.realm.servlet;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.security.realm.AppRealm;
import io.github.greyp9.arwo.core.security.realm.AppRealmContainer;
import io.github.greyp9.arwo.core.security.update.AppRealmFactory;
import io.github.greyp9.arwo.core.security.update.RealmUpdateRunnable;

import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import java.util.concurrent.ExecutorService;

public class RealmServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = -993134841632674349L;

    private transient Context context;
    private transient RealmUpdateRunnable updateRunnable;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        final String contextPath = config.getServletContext().getContextPath();
        context = AppNaming.lookupSubcontext(contextPath);
        final AppRealm appRealm = AppRealmFactory.toAppRealm(contextPath);
        final ExecutorService executor = (ExecutorService) AppNaming.lookup(contextPath, App.EXECUTOR_SERVICE);
        updateRunnable = new RealmUpdateRunnable(context, appRealm);
        executor.execute(updateRunnable);
    }

    @Override
    public final void destroy() {
        updateRunnable.cancel();
        final Object o = AppNaming.lookup(context, AppRealmContainer.NAMING_CONTAINER);
        if (o instanceof AppRealmContainer) {
            final AppRealmContainer appRealmContainer = (AppRealmContainer) o;
            appRealmContainer.setAppRealm(null);
            AppNaming.unbind(context, AppRealmContainer.NAMING_CONTAINER);
        }
        super.destroy();
    }
}
