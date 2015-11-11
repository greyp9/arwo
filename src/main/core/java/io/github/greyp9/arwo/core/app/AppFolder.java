package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.File;
import java.security.Principal;

public final class AppFolder {

    private AppFolder() {
    }

    private static File getArwoRoot() {
        final File arwoHome = new File(SystemU.arwoHome());
        return new File(arwoHome, Const.FOLDER_SETTINGS);
    }

    public static File getWebappRoot(final String contextPath) {
        final File arwoRoot = getArwoRoot();
        return new File(arwoRoot, contextPath);
    }

    public static File getUserHome(final File webappRoot, final Principal principal) {
        final File appHome = new File(webappRoot, "home");
        return new File(appHome, principal.getName());
    }

    private static class Const {
        private static final String FOLDER_SETTINGS = ".arwo";
    }
}
