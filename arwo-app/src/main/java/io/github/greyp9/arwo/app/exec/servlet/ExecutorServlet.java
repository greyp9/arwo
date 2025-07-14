package io.github.greyp9.arwo.app.exec.servlet;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.exec.AppExecutorService;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;

import javax.naming.Context;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import java.io.File;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

@SuppressWarnings("PMD.MoreThanOneLogger")
public class ExecutorServlet extends javax.servlet.http.HttpServlet {
    private static final long serialVersionUID = 2252030217657147780L;

    private transient Context context;
    private transient ExecutorService executorService;
    private transient AppExecutorService appExecutorService;

    @Override
    public final void init(final ServletConfig config) throws ServletException {
        super.init(config);
        final Logger logger = Logger.getLogger(getClass().getName());
        logger.info("init()"); // i18n log
        final String contextPath = config.getServletContext().getContextPath();
        context = AppNaming.lookupSubcontext(contextPath);

        executorService = ExecutorServiceFactory.create(Const.EXECUTOR_SERVICE_NUM_THREAD, getClass().getSimpleName());
        AppNaming.bind(context, App.Naming.EXECUTOR_SERVICE, executorService);

        final File persistDir = FileU.ensureFolder(new File(AppFolder.getWebappRoot(contextPath), "result"));
        appExecutorService = new AppExecutorService(persistDir, Const.EXECUTOR_NUM_THREAD);
        AppNaming.bind(context, App.Naming.EXECUTOR, appExecutorService);
    }

    @SuppressWarnings("PMD.DoNotUseThreads")
    @Override
    public final void destroy() {
        final Logger logger = Logger.getLogger(getClass().getName());
        logger.info(Thread.currentThread().getName());
        synchronized (this) {
            appExecutorService.shutdownNow(Const.STOP_AWAIT_SECS);
            AppNaming.unbind(context, App.Naming.EXECUTOR);
            appExecutorService = null;

            final List<Runnable> runnables = executorService.shutdownNow();
            logger.info(Integer.toString(runnables.size()));
            try {
                executorService.awaitTermination(Const.STOP_AWAIT_SECS, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
            logger.info("awaitTermination()"); // i18n log
            for (final Runnable runnable : runnables) {
                logger.warning(runnable.getClass().getName());
            }
            AppNaming.unbind(context, App.Naming.EXECUTOR_SERVICE);
            executorService = null;
            context = null;
        }
        logger.info("destroy()"); // i18n log
        super.destroy();
    }

    private static class Const {
        private static final int EXECUTOR_SERVICE_NUM_THREAD = 6;  // Realm, Connect, CronService, UserService, 2 extra
        private static final int EXECUTOR_NUM_THREAD = 12;
        private static final int STOP_AWAIT_SECS = 5;
    }
}
