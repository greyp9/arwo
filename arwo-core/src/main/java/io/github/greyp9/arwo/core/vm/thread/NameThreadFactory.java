package io.github.greyp9.arwo.core.vm.thread;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

public class NameThreadFactory implements ThreadFactory {
    private final ThreadGroup threadGroup;
    private final String prefix;
    private final AtomicInteger ordinal;

    public NameThreadFactory(final ThreadGroup threadGroup, final String prefix) {
        this.threadGroup = threadGroup;
        this.prefix = prefix;
        this.ordinal = new AtomicInteger(1);
    }

    @SuppressWarnings("PMD.DoNotUseThreads")
    public final Thread newThread(final Runnable runnable) {
        final String name = String.format("%s-%d", prefix, ordinal.getAndIncrement());
        final Thread thread = new Thread(threadGroup, runnable, name, 0L);
        thread.setDaemon(false);
        return thread;
    }
}
