package io.github.greyp9.arwo.core.date;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

public final class DateU {

    private DateU() {
    }

    public static Date now() {
        return new Date();
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

    public static Date floor(final Date date, final String interval) {
        final Date datePlus = DurationU.add(date, Const.TZ_GMT, interval);
        final long durationMillis = datePlus.getTime() - date.getTime();
        return floor(date, durationMillis);
    }

    public static Date floor(final Date date, final long interval) {
        return ((interval > 0) ? new Date((date.getTime() / interval) * interval) : date);
    }

    public static DateFormat getDateFormat(final String pattern, final TimeZone timeZone, final boolean lenient) {
        final DateFormat dateFormat = new SimpleDateFormat(pattern, Locale.getDefault());
        dateFormat.setTimeZone(timeZone);
        dateFormat.setLenient(lenient);
        return dateFormat;
    }

    public static class Const {
        public static final TimeZone TZ_GMT = TimeZone.getTimeZone("GMT");  // i18n JRE
    }
}
