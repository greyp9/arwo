package io.github.greyp9.arwo.core.date.test;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.util.logging.Logger;

public class DurationSinceTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public final void setUp() throws Exception {
        logger.finest("setUp()");
    }

    @Test
    public void testDurationSinceOneDay() {
        final Date date = new Date();
        final long elapsed = DurationU.Const.ONE_DAY_MILLIS;
        final Date dateBefore = new Date(date.getTime() - elapsed);
        final String duration = DurationU.duration(dateBefore, date);
        Assertions.assertEquals("P1D", duration);
    }

    @Test
    public void testDurationSinceTwentyOneDays() {
        final Date date = new Date();
        logger.finest(XsdDateU.toXSDZMillis(date));
        final long elapsed = DurationU.Const.ONE_DAY_MILLIS * 21;
        final Date dateBefore = new Date(date.getTime() - elapsed);
        logger.finest(XsdDateU.toXSDZMillis(dateBefore));
        final String duration = DurationU.duration(dateBefore, date);
        Assertions.assertEquals("P21D", duration);
    }

    @Test
    public void testDurationSubtractP1Y() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1Y");
        Assertions.assertEquals("1999-01-01T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }

    @Test
    public void testDurationSubtractP1M() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1M");
        Assertions.assertEquals("1999-12-01T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }

    @Test
    public void testDurationSubtractP1D() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1D");
        Assertions.assertEquals("1999-12-31T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }

    @Test
    public void testDurationSubtractP1Y1M1D() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1Y1M1D");
        Assertions.assertEquals("1998-11-30T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }

    @Test
    public void testDurationSubtractP365D() {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1Y");
        Assertions.assertEquals("1999-01-01T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }
}
