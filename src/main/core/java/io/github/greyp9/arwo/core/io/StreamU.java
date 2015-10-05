package io.github.greyp9.arwo.core.io;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

public final class StreamU {

    private StreamU() {
    }

    public static byte[] read(final URL url) throws IOException {
        final BufferedInputStream is = new BufferedInputStream(url.openStream());
        try {
            return read(is);
        } finally {
            is.close();
        }
    }

    @SuppressWarnings("PMD")
    public static byte[] read(final BufferedInputStream is) throws IOException {
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        int b;
        while ((b = is.read()) >= 0) {
            os.write(b);
        }
        return os.toByteArray();
    }
}
