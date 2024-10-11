package io.github.greyp9.arwo.core.text.log.filter.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LogFilterTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private static final String REGEX = "(?sm)"
            + "((^\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2},\\d{3})\\s(.+?))"
            + "(?=(^\\d{4}-\\d{2}-\\d{2})|(?=\n))";

    @Test
    void testSampleA() throws IOException {
        final Pattern pattern = Pattern.compile(REGEX);
        final Pattern patternFoo = Pattern.compile(".*foo.*");
        final Pattern pattern1 = Pattern.compile(".*event1.*");
        final Pattern pattern2 = Pattern.compile(".*event2.*");

        final String text = UTF8Codec.toString(StreamU.read(
                ResourceU.resolve("io/github/greyp9/arwo/text/log/filter/logA.txt")));
        logger.finest(String.format("TEXT LENGTH=%d", text.length()));
        int countMessage = 0;
        int countFoo = 0;
        int count1 = 0;
        int count2 = 0;
        final Matcher matcher = pattern.matcher(text);
        while (matcher.find()) {
            ++countMessage;
            //final String time = matcher.group(1);
            final String message = matcher.group(3);
            countFoo += (patternFoo.matcher(message).matches() ? 1 : 0);
            count1 += (pattern1.matcher(message).matches() ? 1 : 0);
            count2 += (pattern2.matcher(message).matches() ? 1 : 0);
        }
        Assertions.assertEquals(2, countMessage);
        Assertions.assertEquals(2, countFoo);
        Assertions.assertEquals(1, count1);
        Assertions.assertEquals(1, count2);
    }
}
