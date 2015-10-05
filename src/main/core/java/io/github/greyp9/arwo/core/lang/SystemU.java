package io.github.greyp9.arwo.core.lang;

public final class SystemU {
    private static final String USER_HOME = System.getProperty("user.home");

    private SystemU() {
    }

    public static String userHome() {
        return USER_HOME;
    }
}
