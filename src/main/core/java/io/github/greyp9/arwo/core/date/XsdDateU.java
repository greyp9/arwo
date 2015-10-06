package io.github.greyp9.arwo.core.date;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

public final class XsdDateU {

    private XsdDateU() {
    }

    public static Date fromXSDZ(final String date) {
        return ((date == null) ? null : fromXSDZNotNull(date));
    }

    private static Date fromXSDZNotNull(final String dateString) {
        final DateFormat dateFormat = DateU.getDateFormat(Const.XSD_MILLI_Z, DateU.Const.TZ_GMT, true);
        Date date = null;  // NOPMD
        try {
            date = dateFormat.parse(dateString);  // NOPMD
        } catch (ParseException e) {
            e.getClass();
        }
        return date;
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
        private static final String XSD_Z = "yyyy-MM-dd'T'HH:mm:ss'Z'";
        private static final String XSD_MILLI_Z = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";
    }
}
