package io.github.greyp9.arwo.core.vm.thread;

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
}
