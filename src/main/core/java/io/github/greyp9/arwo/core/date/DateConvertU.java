package io.github.greyp9.arwo.core.date;

import java.text.DateFormat;
import java.util.Date;
import java.util.TimeZone;

public final class DateConvertU {

    private DateConvertU() {
    }

    public static Date fromSeconds(final Integer seconds) {
        return ((seconds == null) ? null : fromSeconds((long) seconds));
    }

    public static Date fromSeconds(final Long seconds) {
        return ((seconds == null) ? null : new Date(seconds * DurationU.Const.ONE_SECOND_MILLIS));
    }

    public static Date fromMillis(final Long millis) {
        return ((millis == null) ? null : new Date(millis));
    }

    public static String toString(final Date date, final String tz, final String pattern) {
        return ((date == null) ? null : toStringNotNull(date, tz, pattern));
    }

    private static String toStringNotNull(final Date date, final String tz, final String pattern) {
        final TimeZone timeZone = TimeZone.getTimeZone(tz);
        final DateFormat dateFormat = DateU.getDateFormat(pattern, timeZone, true);
        return dateFormat.format(date);
    }
}
