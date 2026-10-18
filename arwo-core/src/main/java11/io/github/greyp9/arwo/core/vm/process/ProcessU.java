package io.github.greyp9.arwo.core.vm.process;

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
        return process.pid();
    }
}
