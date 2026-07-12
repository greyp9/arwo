package io.github.greyp9.arwo.core.lang;

import java.util.stream.Stream;

public final class ShellU {

    private ShellU() {
    }

    public static String[] toCommandArray(final String stdin) {
        return Stream.concat(getStreamShell(), Stream.of(stdin)).toArray(String[]::new);
    }

    public static String[] toCommandArray(final String[] stdin) {
        return Stream.concat(getStreamShell(), Stream.of(stdin)).toArray(String[]::new);
    }

    private static Stream<String> getStreamShell() {
        final Stream<String> streamShell;
        if (SystemU.isLinux()) {
            streamShell = Stream.of("/bin/sh", "-c");
        } else if (SystemU.isWindows()) {
            streamShell = Stream.of("cmd", "/C");
        } else if (SystemU.isMac()) {
            streamShell = Stream.of("/bin/zsh", "-c");
        } else {
            streamShell = Stream.of();
        }
        return streamShell;
    }
}
