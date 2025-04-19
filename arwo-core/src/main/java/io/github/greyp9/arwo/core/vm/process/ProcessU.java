package io.github.greyp9.arwo.core.vm.process;

import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.logging.Logger;

public final class ProcessU {

    private ProcessU() {
    }

    public static Integer isProcessFinished(final Process process) {
        try {
            return process.exitValue();
        } catch (IllegalThreadStateException e) {
            return null;
        }
    }

    public static Long getProcessId(final Process process) {
        final boolean isUNIXProcess = process.getClass().getName().equals("java.lang.UNIXProcess");
        final boolean isProcessImpl = process.getClass().getName().equals("java.lang.ProcessImpl");
        final Integer pid = (isUNIXProcess || isProcessImpl) ? getProcessIdUNIX(process) : null;
        return (pid == null) ? null : (long) pid;
    }

    private static Integer getProcessIdUNIX(final Process process) {
        final PrivilegedAction<Integer> action = new GetProcessIdPrivilegedAction(process);
        return AccessController.doPrivileged(action);
    }

    public static class GetProcessIdPrivilegedAction implements PrivilegedAction<Integer> {
        private final Process process;

        public GetProcessIdPrivilegedAction(final Process process) {
            this.process = process;
        }

        public final Integer run() {
            Integer processId = null;
            try {
                final Field field = process.getClass().getDeclaredField("pid");  // i18n internal
                field.setAccessible(true);
                processId = ((Integer) field.get(process));
            } catch (ReflectiveOperationException | RuntimeException e) {
                Logger.getLogger(getClass().getName()).warning(e.getMessage());
            }
            return processId;
        }
    }
}
