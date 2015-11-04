package io.github.greyp9.arwo.core.util;

import java.util.Arrays;
import java.util.Collection;

public final class CollectionU {

    private CollectionU() {
    }

    public static Collection<String> toCollection(final String... values) {
        return Arrays.asList(values);
    }
}
