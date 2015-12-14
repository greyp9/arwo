package io.github.greyp9.arwo.app.exec.servlet;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;

import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

@SuppressWarnings("PMD.MoreThanOneLogger")
public class ExecutorServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 2252030217657147780L;

    private transient Context context;
    private transient ExecutorService executorService;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Logger logger = Logger.getLogger(getClass().getName());
        logger.info("init()");
        final String contextPath = config.getServletContext().getContextPath();
        context = AppNaming.lookupSubcontext(contextPath);
        executorService = ExecutorServiceFactory.create(Const.START_NUM_THREAD, getClass().getSimpleName());
        AppNaming.bind(context, App.Naming.EXECUTOR_SERVICE, executorService);
    }

    @SuppressWarnings("PMD.DoNotUseThreads")
    @Override
    public final void destroy() {
        final Logger logger = Logger.getLogger(getClass().getName());
        logger.info(Thread.currentThread().getName());
        synchronized (this) {
            final List<Runnable> runnables = executorService.shutdownNow();
            logger.info(Integer.toString(runnables.size()));
            try {
                executorService.awaitTermination(Const.STOP_AWAIT_SECS, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
            logger.info("awaitTermination()");
            for (final Runnable runnable : runnables) {
                logger.warning(runnable.getClass().getName());
            }
            AppNaming.unbind(context, App.Naming.EXECUTOR_SERVICE);
            executorService = null;
            context = null;
        }
        logger.info("destroy()");
        super.destroy();
    }

    private static class Const {
        private static final int START_NUM_THREAD = 4;  // Realm, Connect, CronService, UserService
        private static final int STOP_AWAIT_SECS = 5;
    }
}
