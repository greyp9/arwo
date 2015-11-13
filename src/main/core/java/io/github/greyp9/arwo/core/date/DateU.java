package io.github.greyp9.arwo.core.date;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

public final class DateU {

    private DateU() {
    }

    public static Date copy(final Date date) {
        return ((date == null) ? null : new Date(date.getTime()));
    }

    public static long since(final Date date) {
        return new Date().getTime() - date.getTime();
    }

    public static Date min(final Date left, final Date right) {
        return (left.before(right) ? left : right);
    }

    public static Date toDate(final Integer seconds) {
        return ((seconds == null) ? null : toDate((long) seconds));
    }

    public static Date toDate(final Long millis) {
        return ((millis == null) ? null : new Date(Const.ONE_SECOND_MILLIS * millis));
    }

    public static String toString(final Date date, final String tz, final String pattern) {
        return ((date == null) ? null : toStringNotNull(date, tz, pattern));
    }

    private static String toStringNotNull(final Date date, final String tz, final String pattern) {
        final TimeZone timeZone = TimeZone.getTimeZone(tz);
        final DateFormat dateFormat = getDateFormat(pattern, timeZone, true);
        return dateFormat.format(date);
    }

    public static DateFormat getDateFormat(final String pattern, final TimeZone timeZone, final boolean lenient) {
        final DateFormat dateFormat = new SimpleDateFormat(pattern, Locale.getDefault());
        dateFormat.setTimeZone(timeZone);
        dateFormat.setLenient(lenient);
        return dateFormat;
    }

    public static class Const {
        public static final TimeZone TZ_GMT = TimeZone.getTimeZone("GMT");

        public static final long ONE_SECOND_MILLIS = 1000L;
    }
}
