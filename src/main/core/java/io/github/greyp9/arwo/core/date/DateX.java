package io.github.greyp9.arwo.core.date;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

public class DateX {
    private final DateFormat dateFormat;

    public DateX(final String pattern, final TimeZone timeZone) {
        this.dateFormat = new SimpleDateFormat(pattern, Locale.getDefault());
        this.dateFormat.setTimeZone(timeZone);
    }

    public final DateFormat getDateFormat() {
        return dateFormat;
    }

    public final Date toDate(final String dateString) {
        return ((dateString == null) ? null : parse(dateFormat, dateString));
    }

    public final String toString(final Date date) {
        return ((date == null) ? null : dateFormat.format(date));
    }

    private static Date parse(final DateFormat dateFormat, final String dateString) {
        Date date = null;  // NOPMD
        try {
            date = dateFormat.parse(dateString);  // NOPMD
        } catch (ParseException e) {
            e.getClass();
        }
        return date;
    }

    public static String toFilenameMM(final Date date) {
        final DateX dateX = Factory.createFilenameMM();
        return ((date == null) ? null : dateX.toString(date));
    }

    public static String toFilename(final Date date) {
        final DateX dateX = Factory.createFilenameMilli();
        return ((date == null) ? null : dateX.toString(date));
    }

    public static Date fromFilenameMM(final String date) {
        final DateX dateX = Factory.createFilenameMM();
        return ((date == null) ? null : dateX.toDate(date));
    }

    public static Date fromFilename(final String date) {
        final DateX dateX = Factory.createFilenameMilli();
        return ((date == null) ? null : dateX.toDate(date));
    }

    public static final class Factory {
        private Factory() {
        }

        public static DateX createURL() {
            return new DateX(Const.URL, DateU.Const.TZ_GMT);
        }

        public static DateX createHttp() {
            return new DateX(Const.HTTP, DateU.Const.TZ_GMT);
        }

        public static DateX createXsdUtc() {
            return new DateX(Const.XSD_UTC, DateU.Const.TZ_GMT);
        }

        public static DateX createXsdUtcMilli() {
            return new DateX(Const.XSD_UTC_MILLI, DateU.Const.TZ_GMT);
        }

        public static DateX createFilenameMM() {
            return new DateX(Const.FILE_UTC, DateU.Const.TZ_GMT);
        }

        public static DateX createFilenameMilli() {
            return new DateX(Const.FILE_UTC_MILLI, DateU.Const.TZ_GMT);
        }

        public static DateX createCustom(final String pattern, final String tz) {
            return new DateX(pattern, TimeZone.getTimeZone(tz));
        }
    }

    private static class Const {
        private static final String URL = "yyyy-MM-dd'T'HH-mm-ss.SSS'Z'";  // i18n internal
        private static final String HTTP = "EEE, dd MMM yyyy HH:mm:ss zzz";  // i18n internal
        private static final String XSD_UTC = "yyyy-MM-dd'T'HH:mm:ss'Z'";  // i18n internal
        private static final String XSD_UTC_MILLI = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";  // i18n internal
        private static final String FILE_UTC = "yyyy-MM-dd'T'HH-mm'Z'";  // i18n internal
        private static final String FILE_UTC_MILLI = "yyyy-MM-dd'T'HH-mm-ss-SSS'Z'";  // i18n internal
    }
}
