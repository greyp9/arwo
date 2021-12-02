package io.github.greyp9.arwo.core.text.line.filter.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.text.filter.TextLineFilter;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.logging.Logger;

/**
 * Exercise line-based filtering of input text using embedded {@link io.github.greyp9.arwo.core.expr.Tree}.
 */
public class TextLineFilterTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testTextLineFilter_Simple() throws IOException {
        final String[] filenames = {
                ".bash_history",
                ".zsh_history",
        };
        for (final String filename : filenames) {
            final File file = new File(SystemU.userHome(), filename);
            if (file.exists()) {
                final byte[] bytesIn = StreamU.read(file);
                final String[] expressions = {
                        "('ls' and not 'ls -la')",
                        "('cd' and not '~')",
                        "('.sh' and 'bin')",
                        "(('git' and not 'github') or 'mvn')",
                };
                for (final String expression : expressions) {
                    final TextFilters textFilters = new TextFilters();
                    textFilters.getExpressions().add(expression);
                    final TextLineFilter filter = new TextLineFilter(textFilters);
                    final byte[] bytesOut = filter.doFilter(bytesIn, StandardCharsets.UTF_8.name());
                    logger.info(expression + SystemU.eol() + UTF8Codec.toString(bytesOut));
                }
            }
        }
    }
}
