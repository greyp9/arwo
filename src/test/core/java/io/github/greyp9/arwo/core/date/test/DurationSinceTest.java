package io.github.greyp9.arwo.core.date.test;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.Assert;
import org.junit.Test;

import java.util.Date;

public class DurationSinceTest {

    @Test
    public void testDurationSinceOneDay() throws Exception {
        final Date date = new Date();
        final long elapsed = DurationU.Const.ONE_DAY_MILLIS;
        final Date dateBefore = new Date(date.getTime() - elapsed);
        final String duration = DurationU.duration(dateBefore, date);
        Assert.assertEquals("P1D", duration);
    }

    @Test
    public void testDurationSinceThirtyDays() throws Exception {
        final Date date = new Date();
        final long elapsed = DurationU.Const.ONE_DAY_MILLIS * 28;
        final Date dateBefore = new Date(date.getTime() - elapsed);
        final String duration = DurationU.duration(dateBefore, date);
        Assert.assertEquals("P28D", duration);
    }

    @Test
    public void testDurationSubtractP1Y() throws Exception {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1Y");
        Assert.assertEquals("1999-01-01T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }

    @Test
    public void testDurationSubtractP1M() throws Exception {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1M");
        Assert.assertEquals("1999-12-01T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }

    @Test
    public void testDurationSubtractP1D() throws Exception {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1D");
        Assert.assertEquals("1999-12-31T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }

    @Test
    public void testDurationSubtractP1Y1M1D() throws Exception {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1Y1M1D");
        Assert.assertEquals("1998-11-30T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }

    @Test
    public void testDurationSubtractP365D() throws Exception {
        final Date date = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateBefore = DurationU.subtract(date, DateU.Const.TZ_GMT, "P1Y");
        Assert.assertEquals("1999-01-01T00:00:00.000Z", XsdDateU.toXSDZMillis(dateBefore));
    }
}
