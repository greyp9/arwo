package io.github.greyp9.arwo.core.lang;

public final class SystemU {

    private SystemU() {
    }

    public static String eol() {
        return Const.LINE_SEPARATOR;
    }

    public static String userHome() {
        return Const.USER_HOME;
    }

    public static String resolve(final String path) {
        return ((path == null) ? null : path.replace("~", userHome()));
    }

    public static String unresolve(final String path) {
        return ((path == null) ? null : path.replace(userHome(), "~"));
    }

    public static void arraycopy(final Object source, final int sourceOffset,
                                 final Object target, final int targetOffset, final int length) {
        System.arraycopy(source, sourceOffset, target, targetOffset, length);
    }

    private static class Const {
        private static final String LINE_SEPARATOR = System.getProperty("line.separator");
        private static final String USER_HOME = System.getProperty("user.home");
    }
}
