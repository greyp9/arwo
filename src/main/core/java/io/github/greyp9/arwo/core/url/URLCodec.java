package io.github.greyp9.arwo.core.url;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

public final class URLCodec {

    private URLCodec() {
    }

    public static URL resolve(final URL url, final String relativeResource) throws IOException {
        try {
            final URI uri = url.toURI();
            final URI resolve = uri.resolve(relativeResource);
            return resolve.toURL();
        } catch (MalformedURLException e) {
            throw new IOException(e);
        } catch (URISyntaxException e) {
            throw new IOException(e);
        }
    }

    public static File toFile(final URL url) throws IOException {
        try {
            return new File(url.toURI());
        } catch (URISyntaxException e) {
            throw new IOException(e);
        }
    }

    public static URL toURL(final String path) throws MalformedURLException {
        return ((path == null) ? null : new URL(path));
    }

    public static URL toURL(final File file) throws MalformedURLException {
        return ((file == null) ? null : file.toURI().toURL());  // NOPMD
    }

    public static String toExternalForm(final URL url) {
        return ((url == null) ? null : url.toExternalForm());
    }
}
