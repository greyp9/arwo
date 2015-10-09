package io.github.greyp9.arwo.core.res;

import java.io.IOException;
import java.net.URL;

public final class ResourceU {

    private ResourceU() {
    }

    @SuppressWarnings("PMD.LawOfDemeter")
    public static URL resolve(final String resourceName) throws IOException {
        final ClassLoader cl = Thread.currentThread().getContextClassLoader();
        return cl.getResource(resourceName);
    }
}
