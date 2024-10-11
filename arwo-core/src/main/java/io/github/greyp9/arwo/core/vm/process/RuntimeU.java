package io.github.greyp9.arwo.core.vm.process;

import java.lang.management.ManagementFactory;

public final class RuntimeU {

    private RuntimeU() {
    }

    public static String getName() {
        return ManagementFactory.getRuntimeMXBean().getName();
    }
}
