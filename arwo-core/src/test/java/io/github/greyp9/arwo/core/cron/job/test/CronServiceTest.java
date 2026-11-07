package io.github.greyp9.arwo.core.cron.job.test;

import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.TimeZone;

public class CronServiceTest {

    @Test
    void testGMT() {
        final TimeZone timeZone = TimeZone.getTimeZone(TZ_GMT);
        Assertions.assertNotNull(timeZone);
        Assertions.assertEquals(TZ_GMT, timeZone.getID());
        Assertions.assertEquals(0, timeZone.getDSTSavings());
        Assertions.assertEquals(0, timeZone.getRawOffset());
        Assertions.assertEquals(0, timeZone.getOffset(XsdDateU.fromXSDZ(DATE_WINTER_N).getTime()));
        Assertions.assertEquals(0, timeZone.getOffset(XsdDateU.fromXSDZ(DATE_SUMMER_N).getTime()));
    }

    @Test
    void testAmerica() {
        final TimeZone timeZone = TimeZone.getTimeZone(TZ_US);
        Assertions.assertNotNull(timeZone);
        Assertions.assertEquals(TZ_US, timeZone.getID());
        Assertions.assertEquals(OFFSET_DST, timeZone.getDSTSavings());
        Assertions.assertEquals(OFFSET_US, timeZone.getRawOffset());
        Assertions.assertEquals(OFFSET_US, timeZone.getOffset(XsdDateU.fromXSDZ(DATE_WINTER_N).getTime()));
        Assertions.assertEquals(OFFSET_US + OFFSET_DST, timeZone.getOffset(XsdDateU.fromXSDZ(DATE_SUMMER_N).getTime()));
    }

    @Test
    void testAsia() {
        final TimeZone timeZone = TimeZone.getTimeZone(TZ_JP);
        Assertions.assertNotNull(timeZone);
        Assertions.assertEquals(TZ_JP, timeZone.getID());
        Assertions.assertEquals(0, timeZone.getDSTSavings());
        Assertions.assertEquals(OFFSET_JP, timeZone.getRawOffset());
        Assertions.assertEquals(OFFSET_JP, timeZone.getOffset(XsdDateU.fromXSDZ(DATE_WINTER_N).getTime()));
        Assertions.assertEquals(OFFSET_JP, timeZone.getOffset(XsdDateU.fromXSDZ(DATE_SUMMER_N).getTime()));
    }


    private static final String TZ_GMT = "GMT";
    private static final String TZ_US = "America/New_York";
    private static final String TZ_JP = "Asia/Tokyo";

    private static final String DATE_WINTER_N = "2000-01-01T00:00:00Z";
    private static final String DATE_SUMMER_N = "2000-07-01T00:00:00Z";

    private static final int OFFSET_DST = 3_600_000;
    private static final int OFFSET_US = -18_000_000;
    private static final int OFFSET_JP = 32_400_000;
}
