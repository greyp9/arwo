package io.github.greyp9.arwo.core.date.test;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

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
        Assertions.assertEquals(MILLENIUM_MILLIS_Z, dateString);
    }

    @Test
    public void testConversionFromMillis() {
        final Date date = XsdDateU.fromXSDZ(MILLENIUM_MILLIS_Z);
        final String dateString = XsdDateU.toXSDZ(date);
        logger.finest(dateString);
        Assertions.assertEquals(MILLENIUM_SECONDS_Z, dateString);
    }

    @Test
    public void testConversionISO() {
        final String dateString = "1970-01-01 00:00:01 +0000";
        final Date date = XsdDateU.fromISO(dateString);
        logger.info(XsdDateU.toXSDZMillis(date));
        Assertions.assertEquals(DurationU.Const.ONE_SECOND_MILLIS, date.getTime());
    }

    @Test
    public void testConversionTZNoDST() {
        final Date date = XsdDateU.fromXSDZ(MILLENIUM_SECONDS_Z);
        final TimeZone tz = TimeZone.getTimeZone(TZ_NY);

        Assertions.assertEquals(OFFSET_TZ_NY, tz.getRawOffset() / (DurationU.Const.ONE_HOUR_MILLIS));
        Assertions.assertEquals(DurationU.Const.ONE_HOUR_MILLIS, tz.getDSTSavings());

        final String dateString = XsdDateU.toXSD(date, tz);
        logger.finest(dateString);
        Assertions.assertEquals("1999-12-31T19:00:00-0500", dateString);
    }

    @Test
    public void testConversionTZWithDST() {
        final Date date = DurationU.add(XsdDateU.fromXSDZ(MILLENIUM_SECONDS_Z), DateU.Const.TZ_GMT, "P6M");
        final TimeZone tz = TimeZone.getTimeZone(TZ_NY);

        final String dateString = XsdDateU.toXSD(date, tz);
        logger.finest(dateString);
        Assertions.assertEquals("2000-06-30T20:00:00-0400", dateString);
    }
}
