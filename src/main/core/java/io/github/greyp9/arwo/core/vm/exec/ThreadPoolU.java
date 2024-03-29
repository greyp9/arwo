package io.github.greyp9.arwo.core.vm.exec;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;

public final class ThreadPoolU {

    private ThreadPoolU() {
    }

    public static String getTelemetry(final ExecutorService executorService) {
        final StringBuilder buffer = new StringBuilder();
        if (executorService instanceof ThreadPoolExecutor) {
            final ThreadPoolExecutor tpe = (ThreadPoolExecutor) executorService;
            buffer.append(String.format("%s,MAX=%d,TASKS=%d,COMPLETED=%d,ACTIVE=%d,QUEUED=%d",
                    tpe.getClass().getSimpleName(), tpe.getMaximumPoolSize(), tpe.getTaskCount(),
                    tpe.getCompletedTaskCount(), tpe.getActiveCount(), tpe.getQueue().size()));
        }
        return buffer.toString();
    }

    public static boolean isAvailablePool(final ExecutorService executorService, final int additionalNeeded) {
        boolean isAvailable = true;
        if (executorService instanceof ThreadPoolExecutor) {
            final ThreadPoolExecutor tpe = (ThreadPoolExecutor) executorService;
            int slack = tpe.getMaximumPoolSize() - tpe.getActiveCount();
            isAvailable = (slack >= additionalNeeded);
        }
        return isAvailable;
    }
}
