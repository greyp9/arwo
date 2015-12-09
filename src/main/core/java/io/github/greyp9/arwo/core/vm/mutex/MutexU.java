package io.github.greyp9.arwo.core.vm.mutex;

import io.github.greyp9.arwo.core.lang.SystemU;

import java.util.Date;

@SuppressWarnings("SynchronizationOnLocalVariableOrMethodParameter")
public final class MutexU {

    private MutexU() {
    }

    public static boolean waitUntil(final Object object, final Date date) {
        boolean interrupted = false;
        final long millis = SystemU.currentTimeMillis();
        if (new Date(millis).before(date)) {
            interrupted = wait(object, date.getTime() - millis);
        }
        return interrupted;
    }

    public static boolean wait(final Object object, final long millis) {
        boolean interrupted = false;
        synchronized (object) {
            try {
                object.wait(millis);
            } catch (InterruptedException e) {
                interrupted = true;
            }
        }
        return interrupted;
    }

    public static void notifyAll(final Object object) {
        synchronized (object) {
            object.notifyAll();
        }
    }
}
