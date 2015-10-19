package io.github.greyp9.arwo.core.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
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

    @SuppressWarnings("PMD.AssignmentInOperand")
    public static byte[] read(final BufferedInputStream is) throws IOException {
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        int b;
        while ((b = is.read()) >= 0) {
            os.write(b);
        }
        return os.toByteArray();
    }

    public static byte[] read(final File file) throws IOException {
        final BufferedInputStream is = new BufferedInputStream(new FileInputStream(file));
        try {
            return read(is);
        } finally {
            is.close();
        }
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static byte[] readSafe(final URL url) {
        try {
            return read(url);
        } catch (IOException e) {
            return null;
        }
    }

    public static void write(final File file, final byte[] bytes) throws IOException {
        final BufferedOutputStream os = new BufferedOutputStream(new FileOutputStream(file));
        try {
            os.write(bytes);
        } finally {
            os.close();
        }
    }
}
