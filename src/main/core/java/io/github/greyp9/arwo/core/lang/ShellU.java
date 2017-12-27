package io.github.greyp9.arwo.core.lang;

import java.util.ArrayList;
import java.util.List;

public final class ShellU {

    public static String[] toCommandArray(final String stdin) {
        final List<String> commandArray = new ArrayList<String>();
        if (SystemU.isLinux()) {
            commandArray.add("/bin/sh");
            commandArray.add("-c");
        } else if (SystemU.isWindows()) {
            commandArray.add("cmd");
            commandArray.add("/C");
        }
        commandArray.add(stdin);
        return commandArray.toArray(new String[commandArray.size()]);
    }
}
