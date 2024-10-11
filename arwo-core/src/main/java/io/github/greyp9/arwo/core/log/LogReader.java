package io.github.greyp9.arwo.core.log;

import io.github.greyp9.arwo.core.charset.UTF8Codec;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class LogReader {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final int chunkSize;
    private final Pattern pattern;
    private final BufferedInputStream bis;

    private final byte[] buffer;
    private final List<String> logs;

    private int cursor;
    private int limit;

    public LogReader(final int chunkSize, final Pattern pattern, final InputStream is) throws IOException {
        this.chunkSize = chunkSize;
        this.pattern = pattern;
        this.bis = new BufferedInputStream(is);
        this.buffer = new byte[chunkSize];
        this.logs = new ArrayList<>();
        processFirstBlock();
    }

    public String getNextLog() throws IOException {
        if (logs.isEmpty()) {
            processNextBlock();
        }
        if (logs.isEmpty()) {
            return null;
        } else {
            return logs.remove(0);
        }
    }

    private void processFirstBlock() throws IOException {
        this.cursor = 0;
        this.limit = 0;
        processNextBlock();
    }

    private void processNextBlock() throws IOException {
        final int bytesRead = bis.read(buffer, limit, chunkSize - limit);
        logger.finest(() -> String.format("read [%d] bytes", bytesRead));
        if (bytesRead >= 0) {
            limit += bytesRead;
        }
        final String text = UTF8Codec.toString(buffer, 0, limit);
        final Matcher matcher = pattern.matcher(text);
        while (matcher.find()) {
            logs.add(matcher.group(0));
            cursor = matcher.end();
        }
        logger.finest(() -> String.format("extracted [%d] logs", logs.size()));
        final int have = limit - cursor;
        //final int need = chunkSize - have;
        System.arraycopy(buffer, cursor, buffer, 0, have);
        cursor = 0;
        limit = have;
        logger.finest(() -> String.format("leftover [%d] bytes", limit));
    }

    public static class Const {
        // recommended chunk size
        public static final int CHUNK_SIZE = 4 * 1024 * 1024;

        // one logback pattern
        public static final String REGEX1 = "(?sm)"
                + "((^\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2},\\d{3})\\s(.+?))"
                + "(?=(^\\d{4}-\\d{2}-\\d{2})|(?=$))";
    }
}
