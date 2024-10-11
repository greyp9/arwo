package io.github.greyp9.arwo.core.vm.exec;

import io.github.greyp9.arwo.core.vm.thread.NameThreadFactory;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

public final class ExecutorServiceFactory {

    private ExecutorServiceFactory() {
    }

    @SuppressWarnings({"PMD.DoNotUseThreads", "PMD.AvoidThreadGroup"})
    public static ExecutorService create(final int nThreads, final String prefix) {
        final ThreadFactory threadFactory = new NameThreadFactory(Thread.currentThread().getThreadGroup(), prefix);
        return Executors.newFixedThreadPool(nThreads, threadFactory);
    }
}
