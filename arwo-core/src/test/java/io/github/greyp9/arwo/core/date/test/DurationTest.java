package io.github.greyp9.arwo.core.date.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Date;

public class DurationTest {

    @Test
    public void testDurationZero() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final String duration = DurationU.duration(date, date);
        Assertions.assertEquals(DurationU.Const.ZERO_SECONDS, duration);
    }

    @Test
    public void testDurationOneMilli() {
        final Date dateEarlier = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateLater = XsdDateU.fromXSDZ("2000-01-01T00:00:00.001Z");
        final String duration = DurationU.duration(dateEarlier, dateLater);
        Assertions.assertEquals("PT0.001S", duration);
    }

    @Test
    public void testDurationOneSecond() {
        final Date dateEarlier = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateLater = XsdDateU.fromXSDZ("2000-01-01T00:00:01.000Z");
        final String duration = DurationU.duration(dateEarlier, dateLater);
        Assertions.assertEquals("PT1S", duration);
    }

    @Test
    public void testDurationFromISO8601() {
        Assertions.assertEquals(1, DurationU.toMillisP("PT0.001S"));
        Assertions.assertEquals(DurationU.Const.HUNDRED_MILLIS, DurationU.toMillisP("PT0.100S"));
        Assertions.assertEquals(DurationU.Const.ONE_SECOND_MILLIS, DurationU.toMillisP("PT1S"));
        Assertions.assertEquals(DurationU.Const.ONE_MINUTE_MILLIS, DurationU.toMillisP("PT1M"));
        Assertions.assertEquals(DurationU.Const.ONE_HOUR_MILLIS, DurationU.toMillisP("PT1H"));
    }
}
