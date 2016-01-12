package io.github.greyp9.arwo.core.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;

public class ReaderU {

    public static String read(BufferedReader reader) throws IOException {
        StringBuilder buffer = new StringBuilder();
        int c;
        while ((c = reader.read()) >= 0) {
            buffer.append((char) c);
        }
        return buffer.toString();
    }

    public static String read(Reader reader) throws IOException {
        BufferedReader bufferedReader = new BufferedReader(reader);
        try {
            return read(bufferedReader);
        } finally {
            reader.close();
        }
    }
}
