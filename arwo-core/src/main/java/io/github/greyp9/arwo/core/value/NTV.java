package io.github.greyp9.arwo.core.value;

public final class NTV {

    private NTV() {
    }

    public static NameTypeValues create(final String... args) {
        return NameTypeValuesU.create(args);
    }
}
