package io.github.greyp9.arwo.core.text.line;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;

public final class LineU {

    private LineU() {
    }

    public static Collection<String> toLines(final String text) throws IOException {
        return toLines(text, Integer.MAX_VALUE);
    }

    public static Collection<String> toLines(final String text, final int maxLines) throws IOException {
        final ArrayList<String> lines = new ArrayList<String>();
        final BufferedReader reader = new BufferedReader(new StringReader((text == null) ? "" : text));
        while (true) {
            final String line = reader.readLine();
            if (line == null) {
                break;
            } else {
                lines.add(line);
                while (lines.size() > maxLines) {
                    lines.remove(0);
                }
            }
        }
        return lines;
    }

/*
    public static String toText(final Collection<String> lines, final String newline) {
        final StringBuilder buffer = new StringBuilder();
        for (final String line : lines) {
            buffer.append(line).append(newline);
        }
        return buffer.toString();
    }

    public static String toTextLines(final String text, final String newline, final int maxLines) throws IOException {
        return toText(toLines(text, maxLines), newline);
    }
*/
}
