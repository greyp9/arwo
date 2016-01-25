package io.github.greyp9.arwo.core.date;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;
import java.util.TimeZone;

public final class HttpDateU {

    private HttpDateU() {
    }

    public static Date fromHttpZ(final String date) {
        return ((date == null) ? null : fromHttpZNotNull(date));
    }

    private static Date fromHttpZNotNull(final String dateString) {
        final DateFormat dateFormat = DateU.getDateFormat(Const.HTTP_Z, DateU.Const.TZ_GMT, true);
        Date date = null;  // NOPMD
        try {
            date = dateFormat.parse(dateString);  // NOPMD
        } catch (ParseException e) {
            e.getClass();
        }
        return date;
    }

    public static String toHttp(final Date date, final String tz) {
        return ((date == null) ? null : toHttpNotNull(date, tz));
    }

    private static String toHttpNotNull(final Date date, final String tz) {
        final TimeZone timeZone = TimeZone.getTimeZone(tz);
        final DateFormat dateFormat = DateU.getDateFormat(Const.HTTP, timeZone, true);
        return dateFormat.format(date);
    }

    public static String toHttpZ(final Date date) {
        return ((date == null) ? null : toHttpZNotNull(date));
    }

    private static String toHttpZNotNull(final Date date) {
        final DateFormat dateFormat = DateU.getDateFormat(Const.HTTP_Z, DateU.Const.TZ_GMT, true);
        return dateFormat.format(date);
    }

    public static class Const {
        private static final String HTTP = "EEE, dd MMM yyyy HH:mm:ss zzz";  // i18n internal
        private static final String HTTP_MILLI = "EEE, dd MMM yyyy HH:mm:ss.SSS zzz";  // i18n internal
        private static final String HTTP_Z = "EEE, dd MMM yyyy HH:mm:ss 'GMT'";  // i18n internal

        public static final String DEFAULT2 = HTTP;
        public static final String DEFAULT = HTTP_MILLI;
    }
}
