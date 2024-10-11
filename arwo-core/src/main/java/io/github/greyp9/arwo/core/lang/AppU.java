package io.github.greyp9.arwo.core.lang;

import io.github.greyp9.arwo.core.value.Value;

public final class AppU {

    private AppU() {
    }

    public static String arwoHome() {
        return Value.defaultOnNull(Const.ARWO_HOME, SystemU.userHome());
    }

    private static class Const {
        private static final String ARWO_HOME = System.getProperty("arwo.home");
    }
}
