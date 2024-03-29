package io.github.greyp9.arwo.core.date;

import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings("PMD.TooManyMethods")
public final class DurationU {

    private DurationU() {
    }

    public static Long toDuration(final Date dateStart, final Date dateFinish, final Date dateIfNotFinished) {
        final Date dateZ = ((dateFinish == null) ? dateIfNotFinished : dateFinish);
        final boolean isValue = ((dateStart != null) && (dateZ != null));
        return (isValue ? (dateZ.getTime() - dateStart.getTime()) : null);
    }

    public static long toMillisP(final String duration, final long defaultMillis) {
        return NumberU.toLong(toMillis(duration), defaultMillis);
    }

    public static long toMillisP(final String duration) {
        return toMillisP(duration, 0L);
    }

    public static Long toMillis(final String duration) {
        final Date start = new Date();
        final Date finish = add(start, DateU.Const.TZ_GMT, duration);
        return ((duration == null) ? null : (finish.getTime() - start.getTime()));
    }

    public static Date add(final Date date, final TimeZone tz, final String durationIn) {
        return add(date, tz, durationIn, 1);
    }

    public static Date subtract(final Date date, final TimeZone tz, final String durationIn) {
        return add(date, tz, durationIn, -1);
    }

    private static Date add(final Date date, final TimeZone tz, final String durationIn, final int sign) {
        final String duration = Value.defaultOnNull(durationIn, "");
        final Calendar calendar = Calendar.getInstance(tz);
        calendar.setTime(date);
        final Matcher matcher = Pattern.compile(Const.PATTERN_DURATION).matcher(duration);
        if (matcher.matches()) {
            add(calendar, matcher.group(Const.GROUP_YEAR), Calendar.YEAR, sign);
            add(calendar, matcher.group(Const.GROUP_MONTH), Calendar.MONTH, sign);
            add(calendar, matcher.group(Const.GROUP_DAY_MONTH), Calendar.DAY_OF_MONTH, sign);
            add(calendar, matcher.group(Const.GROUP_HOUR_DAY), Calendar.HOUR_OF_DAY, sign);
            add(calendar, matcher.group(Const.GROUP_MINUTE), Calendar.MINUTE, sign);
            add(calendar, matcher.group(Const.GROUP_SECOND), Calendar.SECOND, sign);
            add(calendar, matcher.group(Const.GROUP_MILLIS), Calendar.MILLISECOND, sign);
        }
        return calendar.getTime();
    }

    private static void add(final Calendar calendar, final String value, final int field, final int sign) {
        if (value == null) {
            calendar.getClass();
        } else if (field == Calendar.MILLISECOND) {
            calendar.add(field, sign * Integer.parseInt(value.substring(1)));
        } else {
            calendar.add(field, sign * Integer.parseInt(value));
        }
    }

    public static String durationXSD(final long millisIn) {
        long millis = millisIn;
        final long days = millis / Const.ONE_DAY_MILLIS;
        millis %= Const.ONE_DAY_MILLIS;
        final long hours = millis / Const.ONE_HOUR_MILLIS;
        millis %= Const.ONE_HOUR_MILLIS;
        final long minutes = millis / Const.ONE_MINUTE_MILLIS;
        millis %= Const.ONE_MINUTE_MILLIS;
        final long seconds = millis / Const.ONE_SECOND_MILLIS;
        millis %= Const.ONE_SECOND_MILLIS;
        return toDuration(0, 0, days, hours, minutes, seconds, millis);
    }

    public static class Const {
        @SuppressWarnings("PMD.AddEmptyString")
        private static final String PATTERN_DURATION = ""
                + "P((\\d+)Y)?((\\d+)M)?((\\d+)D)?"  // i18n
                + "(T((\\d+)H)?((\\d+)M)?((\\d+)(\\.\\d{3})?S)?)?";

        public static final long TEN_MILLIS = 10L;
        public static final long HUNDRED_MILLIS = 100L;
        public static final long ONE_SECOND_MILLIS = 1000L;
        public static final long ONE_MINUTE_MILLIS = 60L * ONE_SECOND_MILLIS;
        public static final long ONE_HOUR_MILLIS = 60L * ONE_MINUTE_MILLIS;
        public static final long ONE_DAY_MILLIS = 24L * ONE_HOUR_MILLIS;
        public static final long ONE_DAY_HOURS = 24L;
        public static final long ONE_WEEK_DAYS = 7L;

        public static final int GROUP_YEAR = 2;
        public static final int GROUP_MONTH = 4;
        public static final int GROUP_DAY_MONTH = 6;
        public static final int GROUP_HOUR_DAY = 9;
        public static final int GROUP_MINUTE = 11;
        public static final int GROUP_SECOND = 13;
        public static final int GROUP_MILLIS = 14;

        public static final String ONE_DAY = "P1D";  // i18n internal
        public static final String ONE_HOUR = "PT1H";  // i18n internal
        public static final String ONE_MINUTE = "PT1M";  // i18n internal
        public static final String ONE_SECOND = "PT1S";  // i18n internal
        public static final String ZERO_SECONDS = "PT0S";  // i18n internal
    }

/*
    public static String durationXSDZ(final String dateEarlierXSDZ, final String dateLaterXSDZ) {
        final Date dateEarlier = XsdDateU.fromXSDZ(dateEarlierXSDZ);
        final Date dateLater = XsdDateU.fromXSDZ(dateLaterXSDZ);
        return duration(dateEarlier, dateLater);
    }
*/

    public static String duration(final Date dateEarlier, final Date dateLater, final Date dateLaterDefault) {
        final Date dateLaterIt = Value.defaultOnNull(dateLater, dateLaterDefault);
        final boolean isValue = ((dateEarlier != null) && (dateLaterIt != null));
        return (isValue ? duration(dateEarlier, dateLaterIt, Integer.MAX_VALUE) : null);
    }

    public static String duration(final Date dateEarlier, final Date dateLater) {
        return duration(dateEarlier, dateLater, Integer.MAX_VALUE);
    }

    public static String duration(final Date dateEarlier, final Date dateLater, final int significant) {
        return dateEarlier.after(dateLater)
                ? duration(dateLater, dateEarlier, DateU.Const.TZ_GMT, significant)
                : duration(dateEarlier, dateLater, DateU.Const.TZ_GMT, significant);
    }

    public static String duration(final Date dateEarlier, final Date dateLater,
                                  final TimeZone tz, final int significant) {
        final Calendar calendar = Calendar.getInstance(tz);
        calendar.setTime(dateEarlier);
        final long[] atomsD = new long[COUNT_VALUES];
        int i = -1;
        atomsD[++i] = duration(calendar, Calendar.YEAR, dateLater);
        atomsD[++i] = duration(calendar, Calendar.MONTH, dateLater);
        atomsD[++i] = duration(calendar, Calendar.DAY_OF_MONTH, dateLater);
        atomsD[++i] = duration(calendar, Calendar.HOUR_OF_DAY, dateLater);
        atomsD[++i] = duration(calendar, Calendar.MINUTE, dateLater);
        atomsD[++i] = duration(calendar, Calendar.SECOND, dateLater);
        atomsD[++i] = duration(calendar, Calendar.MILLISECOND, dateLater);
        final long[] atomsDS = durationSignificant(significant, atomsD);
        i = -1;
        return toDuration(atomsDS[++i], atomsDS[++i], atomsDS[++i],
                atomsDS[++i], atomsDS[++i], atomsDS[++i], atomsDS[++i]);
    }

    private static final int COUNT_VALUES = 7;

    /**
     * Zero out all of the fields after the number of significant digits.
     */
    private static long[] durationSignificant(final int significant, final long... values) {
        int count = 0;
        for (int i = 0; (i < values.length); ++i) {
            values[i] = (count >= significant) ? 0 : values[i];
            count += (values[i] == 0) ? 0 : 1;
        }
        return values;
    }

    private static int duration(final Calendar calendar, final int field, final Date dateLater) {
        int units = 0;
        while (!calendar.getTime().after(dateLater)) {
            calendar.add(field, 1);
            ++units;
        }
        calendar.add(field, -1);
        return (--units);
    }

    private static String toDuration(final long years, final long months, final long days,
                                     final long hours, final long minutes, final long seconds, final long millis) {
        final boolean t = ((hours != 0) || (minutes != 0) || (seconds != 0) || (millis != 0));
        final boolean p = ((years != 0) || (months != 0) || (days != 0) || (t));
        final StringBuilder buffer = new StringBuilder();
        buffer.append(p ? "P" : "")  // i18n internal
                .append(toAtom(years, "Y"))  // i18n internal
                .append(toAtom(months, "M"))  // i18n internal
                .append(toAtom(days, "D"))  // i18n internal
                .append(t ? "T" : "")  // i18n internal
                .append(toAtom(hours, "H"))  // i18n internal
                .append(toAtom(minutes, "M"));  // i18n internal
        if (millis == 0) {
            buffer.append(toAtom(seconds, "S"));  // i18n internal
        } else {
            buffer.append(String.format("%d.%03dS", seconds, millis));
        }
        if ((!t) && (!p)) {
            buffer.append(Const.ZERO_SECONDS);
        }
        return buffer.toString();
    }

    private static String toAtom(final long units, final String label) {
        return ((units > 0) ? (units + label) : "");
    }
}
