package io.github.greyp9.arwo.core.lang;

import io.github.greyp9.arwo.core.value.Value;

public final class SystemU {

    private SystemU() {
    }

    public static String eol() {
        return Const.LINE_SEPARATOR;
    }

    public static String arwoHome() {
        return Value.defaultOnNull(Const.ARWO_HOME, Const.USER_HOME);
    }

    public static String userHome() {
        return Const.USER_HOME;
    }

    public static String tempDir() {
        return Const.TEMP_DIR;
    }

    public static boolean isTrue() {
        return (!System.getProperties().isEmpty());
    }

    public static String resolve(final String path) {
        return ((path == null) ? null : path.replace("~", userHome()));  // i18n internal
    }

    public static String unresolve(final String path) {
        return ((path == null) ? null : path.replace(userHome(), "~"));  // i18n internal
    }

    public static long currentTimeMillis() {
        return System.currentTimeMillis();
    }

    @SuppressWarnings("SuspiciousSystemArraycopy")
    public static void arraycopy(final Object source, final int sourceOffset,
                                 final Object target, final int targetOffset, final int length) {
        System.arraycopy(source, sourceOffset, target, targetOffset, length);
    }

    private static class Const {
        private static final String LINE_SEPARATOR = System.getProperty("line.separator");
        private static final String ARWO_HOME = System.getProperty("arwo.home");
        private static final String USER_HOME = System.getProperty("user.home");
        private static final String TEMP_DIR = System.getProperty("java.io.tmpdir");
    }
}
