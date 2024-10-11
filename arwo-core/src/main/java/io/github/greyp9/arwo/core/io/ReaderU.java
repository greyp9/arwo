package io.github.greyp9.arwo.core.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;

public final class ReaderU {

    private ReaderU() {
    }

    @SuppressWarnings("PMD.AssignmentInOperand")
    public static String read(final BufferedReader reader) throws IOException {
        final StringBuilder buffer = new StringBuilder();
        int c;
        while ((c = reader.read()) >= 0) {
            buffer.append((char) c);
        }
        return buffer.toString();
    }

    public static String read(final Reader reader) throws IOException {
        final BufferedReader bufferedReader = new BufferedReader(reader);
        try {
            return read(bufferedReader);
        } finally {
            reader.close();
        }
    }
}
