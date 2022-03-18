package io.github.greyp9.arwo.core.text.line.filter.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.text.filter.TextLineFilter;
import org.junit.Assert;
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
                    logger.finest(expression + SystemU.eol() + UTF8Codec.toString(bytesOut));
                }
            }
        }
    }

    @Test
    public void testTextLineFilter_Regex() throws IOException {
        final String[] filenames = {
                ".bash_history",
                ".zsh_history",
        };
        for (final String filename : filenames) {
            final File file = new File(SystemU.userHome(), filename);
            if (file.exists()) {
                final byte[] bytesIn = StreamU.read(file);
                final String[] expressions = {
                        "regex('ls.*')",
                        "regex('ls.+')",
                };
                for (final String expression : expressions) {
                    final TextFilters textFilters = new TextFilters();
                    textFilters.getExpressions().add(expression);
                    final TextLineFilter filter = new TextLineFilter(textFilters);
                    final byte[] bytesOut = filter.doFilter(bytesIn, StandardCharsets.UTF_8.name());
                    logger.finest(expression + SystemU.eol() + UTF8Codec.toString(bytesOut));
                }
            }
        }
    }

    @Test
    public void testTextLineFilter_RegexSamples1() throws IOException {
        final TextFilters textFilters = new TextFilters();
        textFilters.getExpressions().add("regex('\\d{3}')");
        final TextLineFilter filter = new TextLineFilter(textFilters);
        Assert.assertTrue(filter.doFilter(UTF8Codec.toBytes("1234"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertTrue(filter.doFilter(UTF8Codec.toBytes("123"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertFalse(filter.doFilter(UTF8Codec.toBytes("12"), UTF8Codec.Const.UTF8).length > 0);
    }

    @Test
    public void testTextLineFilter_RegexSamples2() throws IOException {
        final TextFilters textFilters = new TextFilters();
        textFilters.getExpressions().add("regex('\\w{3}')");
        final TextLineFilter filter = new TextLineFilter(textFilters);
        Assert.assertTrue(filter.doFilter(UTF8Codec.toBytes("abcd"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertTrue(filter.doFilter(UTF8Codec.toBytes("abc"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertFalse(filter.doFilter(UTF8Codec.toBytes("ab"), UTF8Codec.Const.UTF8).length > 0);
    }

    @Test
    public void testTextLineFilter_RegexSamples3() throws IOException {
        final TextFilters textFilters = new TextFilters();
        textFilters.getExpressions().add("regex('\\w+?\\s+?\\w+?')");
        final TextLineFilter filter = new TextLineFilter(textFilters);
        Assert.assertTrue(filter.doFilter(UTF8Codec.toBytes("ab cd"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertTrue(filter.doFilter(UTF8Codec.toBytes("ab c"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertFalse(filter.doFilter(UTF8Codec.toBytes("ab"), UTF8Codec.Const.UTF8).length > 0);
    }

    @Test
    public void testTextLineFilter_RegexSamples4() throws IOException {
        final TextFilters textFilters = new TextFilters();
        textFilters.getExpressions().add("regex('[abc]{3}')");
        final TextLineFilter filter = new TextLineFilter(textFilters);
        Assert.assertFalse(filter.doFilter(UTF8Codec.toBytes("bcdbcd"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertTrue(filter.doFilter(UTF8Codec.toBytes("abcd"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertTrue(filter.doFilter(UTF8Codec.toBytes("abab"), UTF8Codec.Const.UTF8).length > 0);
        Assert.assertFalse(filter.doFilter(UTF8Codec.toBytes("ab"), UTF8Codec.Const.UTF8).length > 0);
    }
}
