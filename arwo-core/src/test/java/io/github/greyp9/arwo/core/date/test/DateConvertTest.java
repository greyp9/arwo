package io.github.greyp9.arwo.core.date.test;

import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.util.logging.Logger;

public class DateConvertTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testConversionToMillis() {
        final String dateString = "Sat, 01 Jan 2000 00:00:00.000 GMT";
        final Date date = HttpDateU.fromHttpZMilli(dateString);
        final String dateXSD = XsdDateU.toXSDZ(date);
        logger.finest(dateXSD);
        Assertions.assertEquals("2000-01-01T00:00:00Z", dateXSD);
    }

    @Test
    public void testConversionFromSeconds() {
        final Date date = new Date(1735689600000L);
        final String dateXSD = XsdDateU.toXSDZ(date);
        logger.finest(dateXSD);
        Assertions.assertEquals("2025-01-01T00:00:00Z", dateXSD);
    }
}
