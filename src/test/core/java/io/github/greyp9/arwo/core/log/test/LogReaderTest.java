package io.github.greyp9.arwo.core.log.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.log.LogReader;
import io.github.greyp9.arwo.core.res.ResourceU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.regex.Pattern;

public class LogReaderTest {
    private static final int CHUNK_SIZE = LogReader.Const.CHUNK_SIZE;
    private static final String REGEX = LogReader.Const.REGEX1;

    @Test
    void testEmpty() throws IOException {
        final byte[] bytes = new byte[0];
        final LogReader logReader = new LogReader(CHUNK_SIZE, Pattern.compile(REGEX), new ByteArrayInputStream(bytes));
        Assertions.assertNull(logReader.getNextLog());
    }

    @Test
    void testInvalid() throws IOException {
        final byte[] bytes = UTF8Codec.toBytes("not a log line");
        final LogReader logReader = new LogReader(CHUNK_SIZE, Pattern.compile(REGEX), new ByteArrayInputStream(bytes));
        Assertions.assertNull(logReader.getNextLog());
    }

    @Test
    void testInvalidNewline() throws IOException {
        final byte[] bytes = UTF8Codec.toBytes("not a log line\n");
        final LogReader logReader = new LogReader(CHUNK_SIZE, Pattern.compile(REGEX), new ByteArrayInputStream(bytes));
        Assertions.assertNull(logReader.getNextLog());
    }

    @Test
    void testSampleHappyPath() throws IOException {
        final byte[] bytes = StreamU.read(ResourceU.resolve("io/github/greyp9/arwo/text/log/filter/logA.txt"));
        final LogReader logReader = new LogReader(CHUNK_SIZE, Pattern.compile(REGEX), new ByteArrayInputStream(bytes));
        Assertions.assertEquals("2024-01-01 19:00:00,001 ERROR foo: event1", logReader.getNextLog());
        Assertions.assertEquals("2024-01-01 19:00:00,002 ERROR foo: event2", logReader.getNextLog());
        Assertions.assertNull(logReader.getNextLog());
    }

    @Test
    void testSampleMultiline() throws IOException {
        final byte[] bytes = StreamU.read(ResourceU.resolve("io/github/greyp9/arwo/text/log/filter/logC.txt"));
        final LogReader logReader = new LogReader(CHUNK_SIZE, Pattern.compile(REGEX), new ByteArrayInputStream(bytes));
        Assertions.assertTrue(logReader.getNextLog().contains("event1"));
        Assertions.assertTrue(logReader.getNextLog().contains("event2"));
        Assertions.assertNull(logReader.getNextLog());
    }

    @Test
    void testSampleEmbedDate() throws IOException {
        final byte[] bytes = StreamU.read(ResourceU.resolve("io/github/greyp9/arwo/text/log/filter/logD.txt"));
        final LogReader logReader = new LogReader(CHUNK_SIZE, Pattern.compile(REGEX), new ByteArrayInputStream(bytes));
        Assertions.assertTrue(logReader.getNextLog().contains("event1"));
        Assertions.assertTrue(logReader.getNextLog().contains("event2"));
        Assertions.assertNull(logReader.getNextLog());
    }

    private static final int CHUNK_SMALL = 256;

    @Test
    void testSamplePages() throws IOException {
        final byte[] bytes = StreamU.read(ResourceU.resolve("io/github/greyp9/arwo/text/log/filter/logB.txt"));
        final LogReader logReader = new LogReader(CHUNK_SMALL, Pattern.compile(REGEX), new ByteArrayInputStream(bytes));
        Assertions.assertTrue(logReader.getNextLog().contains("event1"));
        Assertions.assertTrue(logReader.getNextLog().contains("event2"));
        Assertions.assertTrue(logReader.getNextLog().contains("event3"));
        Assertions.assertTrue(logReader.getNextLog().contains("event4"));
        Assertions.assertTrue(logReader.getNextLog().contains("event5"));
        Assertions.assertTrue(logReader.getNextLog().contains("event6"));
        Assertions.assertNull(logReader.getNextLog());
    }
}
