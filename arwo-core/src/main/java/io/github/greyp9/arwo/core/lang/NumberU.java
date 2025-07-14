package io.github.greyp9.arwo.core.lang;

import java.text.NumberFormat;
import java.text.ParseException;

public final class NumberU {

    private NumberU() {
    }

    public static int toInt(final Integer i, final int defaultOnNull) {
        return (i == null) ? defaultOnNull : i;
    }

    public static long toLong(final Long l, final long defaultOnNull) {
        return (l == null) ? defaultOnNull : l;
    }

    public static Integer toInteger(final String s) {
        try {
            return ((s == null) ? null : Integer.valueOf(s));
        } catch (NumberFormatException e) {
            return null;
        }
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static int toInt(final String s, final int defaultInt) {
        try {
            final Number number = ((s == null) ? defaultInt : NumberFormat.getInstance().parse(s));
            return number.intValue();
        } catch (ParseException e) {
            return defaultInt;
        }
    }

    public static float toFloat(final String s, final float defaultFloat) {
        try {
            final Number number = ((s == null) ? defaultFloat : NumberFormat.getInstance().parse(s));
            return number.floatValue();
        } catch (ParseException e) {
            return defaultFloat;
        }
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Long toLong(final String s, final long defaultLong) {
        try {
            final Number number = ((s == null) ? defaultLong : NumberFormat.getInstance().parse(s));
            return number.longValue();
        } catch (ParseException e) {
            return defaultLong;
        }
    }

    public static boolean inBounds(final int number, final int min, final int max) {
        return ((number >= min) && (number <= max));
    }

    public static String toHex(final int value) {
        return String.format("%08x", value);  // i18n internal
    }

    public static class Const {
        public static final int RADIX_OCTAL = 8;
        public static final int RADIX_HEX = 16;
    }
}
