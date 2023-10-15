package io.github.greyp9.arwo.core.date.test;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.Assert;
import org.junit.Test;

import java.util.Date;
import java.util.TimeZone;
import java.util.logging.Logger;

public class XsdDateUTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private static final String MILLENIUM_SECONDS_Z = "2000-01-01T00:00:00Z";
    private static final String MILLENIUM_MILLIS_Z = "2000-01-01T00:00:00.000Z";
    private static final String TZ_NY = "America/New_York";
    private static final int OFFSET_TZ_NY = -5;

    @Test
    public void testConversionToMillis() {
        final Date date = XsdDateU.fromXSDZ(MILLENIUM_SECONDS_Z);
        final String dateString = XsdDateU.toXSDZMillis(date);
        logger.finest(dateString);
        Assert.assertEquals(MILLENIUM_MILLIS_Z, dateString);
    }

    @Test
    public void testConversionFromMillis() {
        final Date date = XsdDateU.fromXSDZ(MILLENIUM_MILLIS_Z);
        final String dateString = XsdDateU.toXSDZ(date);
        logger.finest(dateString);
        Assert.assertEquals(MILLENIUM_SECONDS_Z, dateString);
    }

    @Test
    public void testConversionTZNoDST() {
        final Date date = XsdDateU.fromXSDZ(MILLENIUM_SECONDS_Z);
        final TimeZone tz = TimeZone.getTimeZone(TZ_NY);

        Assert.assertEquals(OFFSET_TZ_NY, tz.getRawOffset() / (DurationU.Const.ONE_HOUR_MILLIS));
        Assert.assertEquals(DurationU.Const.ONE_HOUR_MILLIS, tz.getDSTSavings());

        final String dateString = XsdDateU.toXSD(date, tz);
        logger.finest(dateString);
        Assert.assertEquals("1999-12-31T19:00:00-0500", dateString);
    }

    @Test
    public void testConversionTZWithDST() {
        final Date date = DurationU.add(XsdDateU.fromXSDZ(MILLENIUM_SECONDS_Z), DateU.Const.TZ_GMT, "P6M");
        final TimeZone tz = TimeZone.getTimeZone(TZ_NY);

        final String dateString = XsdDateU.toXSD(date, tz);
        logger.finest(dateString);
        Assert.assertEquals("2000-06-30T20:00:00-0400", dateString);
    }
}
