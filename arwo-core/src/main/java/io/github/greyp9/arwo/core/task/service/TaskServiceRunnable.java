package io.github.greyp9.arwo.core.task.service;

import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.task.config.TaskServiceConfig;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import javax.naming.Context;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

public final class TaskServiceRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final TaskService taskService;
    private final AtomicReference<String> reference;
    private final long interval;

    public TaskService getTaskService() {
        return taskService;
    }

    public TaskServiceRunnable(final TaskServiceConfig config,
                               final AtomicReference<String> reference,
                               final long interval) {
        this.taskService = new TaskService(config);
        this.reference = reference;
        this.interval = interval;
        logger.info("READY");
    }

    public static TaskServiceRunnable create(final TaskServiceConfig config,
                                             final AtomicReference<String> reference,
                                             final long interval) {
        return new TaskServiceRunnable(config, reference, interval);
    }

    @Override
    public void run() {
        // register TaskService in the InitialContext
        final Context context = AppNaming.createSubcontext(TaskService.class.getName());
        AppNaming.bind(context, taskService.getName(), taskService);

        // await shutdown signal from application
        final String methodName = String.format("run(%d)", 0);
        logger.info("ENTER:" + methodName);
        logger.entering(getClass().getSimpleName(), methodName);
        while (reference.get() == null) {
            ThreadU.sleepMillis(interval);
        }
        MutexU.notifyAll(reference);

        // unregister TaskService
        AppNaming.unbind(context, taskService.getName());
        logger.info("EXIT:" + methodName);
        logger.exiting(getClass().getSimpleName(), methodName);
    }
}
