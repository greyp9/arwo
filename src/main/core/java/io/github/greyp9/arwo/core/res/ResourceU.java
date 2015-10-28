package io.github.greyp9.arwo.core.res;

import java.io.IOException;
import java.net.URL;

public final class ResourceU {

    private ResourceU() {
    }

    @SuppressWarnings("PMD.DoNotUseThreads")
    public static URL resolve(final String resourceName) throws IOException {
        final Thread thread = Thread.currentThread();
        final ClassLoader classLoader = thread.getContextClassLoader();
        return classLoader.getResource(resourceName);
    }
}
