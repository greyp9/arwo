package io.github.greyp9.arwo.core.vm.process;

import java.lang.management.ManagementFactory;

public final class RuntimeU {

    private RuntimeU() {
    }

    public static String getName() {
        // https://stackoverflow.com/questions/41512483/runtimemxbean-getname-hangs-on-mac-os-x-sierra-how-to-fix
        // $ scutil --set HostName $(scutil --get LocalHostName)
        return ManagementFactory.getRuntimeMXBean().getName();
    }
}
