package io.github.greyp9.arwo.core.vm.thread;

import io.github.greyp9.arwo.core.lang.SystemU;

import java.util.Date;

public final class ThreadU {

    private ThreadU() {
    }

    @SuppressWarnings("PMD.AvoidCatchingThrowable")
    public static boolean sleepMillis(final long millis) {
        boolean interrupted = false;
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            interrupted = true;
        } catch (Throwable t) {
            interrupted = true;
        }
        return interrupted;
    }

    public static boolean sleepUntil(final Date date) {
        boolean interrupted = false;
        long millis = SystemU.currentTimeMillis();
        while ((!interrupted) && (date.getTime() > millis)) {
            interrupted = sleepMillis(date.getTime() - millis);
            millis = SystemU.currentTimeMillis();
        }
        return interrupted;
    }
}
