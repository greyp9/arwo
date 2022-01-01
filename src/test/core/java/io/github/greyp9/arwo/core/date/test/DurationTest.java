package io.github.greyp9.arwo.core.date.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.Assert;
import org.junit.Test;

import java.util.Date;

public class DurationTest {

    @Test
    public void testDurationZero() throws Exception {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final String duration = DurationU.duration(date, date);
        Assert.assertEquals(DurationU.Const.ZERO_SECONDS, duration);
    }

    @Test
    public void testDurationOneMilli() throws Exception {
        final Date dateEarlier = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateLater = XsdDateU.fromXSDZ("2000-01-01T00:00:00.001Z");
        final String duration = DurationU.duration(dateEarlier, dateLater);
        Assert.assertEquals("PT0.001S", duration);
    }

    @Test
    public void testDurationOneSecond() throws Exception {
        final Date dateEarlier = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateLater = XsdDateU.fromXSDZ("2000-01-01T00:00:01.000Z");
        final String duration = DurationU.duration(dateEarlier, dateLater);
        Assert.assertEquals("PT1S", duration);
    }
}
