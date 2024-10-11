package io.github.greyp9.arwo.core.date.test;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Date;

public class DurationResolutionTest {

    @Test
    public void testDurationSignificant() throws Exception {
        final Date dateBefore = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        final Date dateAfter = XsdDateU.fromXSDZ("2001-02-02T01:01:01.001Z");
        int significant = 0;
        Assertions.assertEquals("P1Y", DurationU.duration(dateBefore, dateAfter, ++significant));
        Assertions.assertEquals("P1Y1M", DurationU.duration(dateBefore, dateAfter, ++significant));
        Assertions.assertEquals("P1Y1M1D", DurationU.duration(dateBefore, dateAfter, ++significant));
        Assertions.assertEquals("P1Y1M1DT1H", DurationU.duration(dateBefore, dateAfter, ++significant));
        Assertions.assertEquals("P1Y1M1DT1H1M", DurationU.duration(dateBefore, dateAfter, ++significant));
        Assertions.assertEquals("P1Y1M1DT1H1M1S", DurationU.duration(dateBefore, dateAfter, ++significant));
        Assertions.assertEquals("P1Y1M1DT1H1M1.001S", DurationU.duration(dateBefore, dateAfter, ++significant));
    }
}
