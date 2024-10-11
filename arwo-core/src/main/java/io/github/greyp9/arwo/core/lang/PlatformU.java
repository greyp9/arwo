package io.github.greyp9.arwo.core.lang;

import java.io.File;
import java.net.URISyntaxException;

public final class PlatformU {

    private PlatformU() {
    }

/*
    public static File getCodeJar(Class<?> c) throws URISyntaxException {
        final File codeLocation = getCodeLocation(c);
        return (codeLocation.isFile() ? codeLocation : null);
    }
*/

    public static File getCodeFolder(final Class<?> c) throws URISyntaxException {
        final File codeLocation = getCodeLocation(c);
        return (codeLocation.isFile() ? codeLocation.getParentFile() : codeLocation);
    }

    public static File getCodeLocation(final Class<?> c) throws URISyntaxException {
        return new File(c.getProtectionDomain().getCodeSource().getLocation().toURI());
    }
}
