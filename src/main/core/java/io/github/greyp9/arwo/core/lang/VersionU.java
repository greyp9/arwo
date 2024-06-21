package io.github.greyp9.arwo.core.lang;

import io.github.greyp9.arwo.core.value.Value;

import java.util.List;

public final class VersionU {

    private VersionU() {
    }

    public static int languageLevel() {
        return LANGUAGE_LEVEL;
    }

    public static int languageLevel(final String javaVersion) {
        final List<String> values = Value.split(".", javaVersion);
        final String first = values.isEmpty() ? "" : values.get(0);
        final String token = (first.equals("1") ? values.get(1) : first);
        return NumberU.toInt(token, 0);
    }

    private static final String JAVA_VERSION = System.getProperty("java.version");
    private static final int LANGUAGE_LEVEL = languageLevel(JAVA_VERSION);
}
