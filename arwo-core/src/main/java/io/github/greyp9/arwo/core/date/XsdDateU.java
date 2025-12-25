package io.github.greyp9.arwo.core.date;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;
import java.util.TimeZone;

public final class XsdDateU {

    private XsdDateU() {
    }

    public static Date fromXSDZ(final String date) {
        return ((date == null) ? null : fromXSDZNotNull(date));
    }

    private static Date fromXSDZNotNull(final String dateString) {
        final boolean lenient = (dateString.length() > Const.LENGTH_XSD_Z);
        final String pattern = (lenient ? Const.XSD_MILLI_Z : Const.XSD_Z);
        final DateFormat dateFormat = DateU.getDateFormat(pattern, DateU.Const.TZ_GMT, lenient);
        Date date = null;  // NOPMD
        try {
            date = dateFormat.parse(dateString);  // NOPMD
        } catch (ParseException e) {
            e.getClass();
        }
        return date;
    }

    public static Date fromISO(final String date) {
        return ((date == null) ? null : fromISONotNull(date));
    }

    private static Date fromISONotNull(final String dateString) {
        final DateFormat dateFormat = DateU.getDateFormat(Const.ISO, DateU.Const.TZ_GMT, true);
        Date date = null;
        try {
            date = dateFormat.parse(dateString);
        } catch (ParseException e) {
            e.getClass();
        }
        return date;
    }

    public static String toXSDwDOW(final Date date, final TimeZone timeZone) {
        final String pattern = "yyyy-MM-dd'T'HH:mm:ssZ/EEE";
        final DateFormat dateFormat = DateU.getDateFormat(pattern, timeZone, true);
        return dateFormat.format(date);
    }

    public static String toXSD(final Date date, final TimeZone timeZone) {
        final String pattern = "yyyy-MM-dd'T'HH:mm:ssZ";
        final DateFormat dateFormat = DateU.getDateFormat(pattern, timeZone, true);
        return dateFormat.format(date);
    }

    public static String toXSDZ(final Date date) {
        return ((date == null) ? null : toXSDZNotNull(date, false));
    }

    public static String toXSDZMillis(final Date date) {
        return ((date == null) ? null : toXSDZNotNull(date, true));
    }

    private static String toXSDZNotNull(final Date date, final boolean millis) {
        final String pattern = ((millis) ? Const.XSD_MILLI_Z : Const.XSD_Z);
        final DateFormat dateFormat = DateU.getDateFormat(pattern, DateU.Const.TZ_GMT, true);
        return dateFormat.format(date);
    }

    public static class Const {
        private static final String XSD_Z = "yyyy-MM-dd'T'HH:mm:ss'Z'";  // i18n internal
        private static final String XSD_MILLI_Z = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";  // i18n internal
        private static final int LENGTH_XSD_Z = 20;

        private static final String ISO = "yyyy-MM-dd HH:mm:ss Z";

        public static final String DATE = "yyyy-MM-dd";  // i18n internal
        public static final String TIME = "HH:mm:ss'Z'";  // i18n internal
        public static final String DATETIME = XSD_Z;
    }
}
