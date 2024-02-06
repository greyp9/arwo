package io.github.greyp9.arwo.core.vm.process;

public final class ProcessU {

    private ProcessU() {
    }

    public static Long getProcessId(final Process process) {
        return process.pid();
    }
}
