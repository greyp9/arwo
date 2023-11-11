package io.github.greyp9.arwo.core.locus.test;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.locus.Locus;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

public class LocusTest {

    @Test
    public void testDate() throws Exception {
        DateX dateX = new DateX(HttpDateU.Const.DEFAULT2, TimeZone.getTimeZone("UTC"));
        Date date2000 = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        Assertions.assertEquals("Sat, 01 Jan 2000 00:00:00 UTC", dateX.toString(date2000));
        Date date2001 = XsdDateU.fromXSDZ("2001-01-01T00:00:00.000Z");
        Assertions.assertEquals("Mon, 01 Jan 2001 00:00:00 UTC", dateX.toString(date2001));
    }

    @Test
    public void testLocus() throws Exception {
        DateX dateX = new DateX(HttpDateU.Const.DEFAULT2, TimeZone.getTimeZone("UTC"));
        Locus locus = new Locus(Locale.getDefault(), dateX);
        // date
        Date date2000 = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        Assertions.assertEquals("Sat, 01 Jan 2000 00:00:00 UTC", locus.toString(date2000));
        Date date2001 = XsdDateU.fromXSDZ("2001-01-01T00:00:00.000Z");
        Assertions.assertEquals("Mon, 01 Jan 2001 00:00:00 UTC", locus.toString(date2001));
        // integer
        Assertions.assertEquals("1,000,000", locus.toString(locus.toInt("1000000")));
        Assertions.assertEquals("1,000,000", locus.toString(locus.toInt("1,000,000")));
        // long
        Assertions.assertEquals("1,000,000", locus.toString(locus.toLong("1000000")));
        Assertions.assertEquals("1,000,000", locus.toString(locus.toLong("1,000,000")));
    }
}
