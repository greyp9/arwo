package io.github.greyp9.arwo.core.exec;

import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public final class AppExecutorService {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final File persistDir;
    private final ExecutorService executorService;
    private final List<Future<?>> futures;

    public File getPersistDir() {
        return persistDir;
    }

    public ExecutorService getExecutorService() {
        return executorService;
    }

    public List<Future<?>> getFutures() {
        return futures;
    }

    public List<Future<?>> getFuturesIncomplete() {
        return futures.stream().filter(f -> !f.isDone()).collect(Collectors.toList());
    }

    public AppExecutorService(final File persistDir, final int nThreads) {
        this.persistDir = persistDir;
        this.executorService = ExecutorServiceFactory.create(nThreads, getClass().getSimpleName());
        this.futures = new ArrayList<>();
        logger.info("READY");
    }


    public void submit(final Runnable runnable) {
        final Future<?> future = executorService.submit(runnable);
        futures.add(future);
        logger.info(String.format("SUBMIT: %s", future));
    }

    public boolean shutdownNow(final int secondsWait) {
        logger.info("STOP");
        final List<Runnable> runnables = this.executorService.shutdownNow();
        logger.info(String.format("STOP, wait for %d runnables", runnables.size()));
        boolean awaitTermination = false;
        try {
            awaitTermination = executorService.awaitTermination(secondsWait, TimeUnit.SECONDS);
            logger.info(String.format("STOP, awaitTermination = %s", awaitTermination));
        } catch (InterruptedException e) {
            logger.severe(e.getMessage());
        }
        return awaitTermination;
    }
}
