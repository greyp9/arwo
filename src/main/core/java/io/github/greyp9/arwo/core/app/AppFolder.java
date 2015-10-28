package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.File;

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

    private static class Const {
        private static final String FOLDER_SETTINGS = ".arwo";
    }
}
