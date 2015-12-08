package io.github.greyp9.arwo.core.text.filter;

import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Collection;

public class TextLineFilter {
    private final Collection<String> includes;
    private final Collection<String> excludes;

    public TextLineFilter(final TextFilters textFilters) {
        this.includes = textFilters.getIncludes();
        this.excludes = textFilters.getExcludes();
    }

    public final byte[] doFilter(final byte[] bytes, final String encoding) throws IOException {
        final BufferedReader reader = new BufferedReader(
                new InputStreamReader(new ByteArrayInputStream(bytes), encoding));
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(os, encoding));
        while (true) {
            final String line = reader.readLine();
            if (line == null) {
                break;
            } else if (matchesTextFilters(line)) {
                writer.write(line);
                writer.write(SystemU.eol());
            }
        }
        writer.flush();
        writer.close();
        return os.toByteArray();
    }

    private boolean matchesTextFilters(final String line) {
        boolean matches = true;
        for (final String include : includes) {
            if ((matches) && (!line.contains(include))) {
                matches = false;
            }
        }
        for (final String exclude : excludes) {
            if ((matches) && (line.contains(exclude))) {
                matches = false;
            }
        }
        return matches;
    }
}
