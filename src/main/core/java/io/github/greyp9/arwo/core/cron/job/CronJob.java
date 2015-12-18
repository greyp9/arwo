package io.github.greyp9.arwo.core.cron.job;

import io.github.greyp9.arwo.core.date.DurationU;

import java.util.BitSet;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

public class CronJob {
    // job config
    private final String name;
    private final String line;
    // derived
    private final BitSet minute;
    private final BitSet hour;
    private final BitSet day;
    private final BitSet month;
    private final BitSet dayOfWeek;
    private final String command;

    public final String getName() {
        return name;
    }

    public final String getLine() {
        return line;
    }

    public final String getCommand() {
        return command;
    }

    @SuppressWarnings({ "PMD.ExcessiveParameterList", "checkstyle:parameternumber" })
    public CronJob(final String name, final String line, final BitSet minute, final BitSet hour,
                   final BitSet day, final BitSet month, final BitSet dayOfWeek, final String command) {
        this.name = name;
        this.line = line;
        this.minute = minute;
        this.hour = hour;
        this.day = day;
        this.month = month;
        this.dayOfWeek = dayOfWeek;
        this.command = command;
    }

    public final Date getDateNext(final Date date, final TimeZone tz, final String duration) {
        Date dateNext = null;
        final Date dateUntil = DurationU.add(date, tz, duration);
        final Calendar calendar = Calendar.getInstance(tz);
        calendar.setTime(date);
        calendar.set(Calendar.MILLISECOND, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.add(Calendar.MINUTE, 1);
        // iterate over range
        while (!calendar.getTime().after(dateUntil)) {
            if (isTime(calendar)) {
                dateNext = calendar.getTime();
                break;
            }
            calendar.add(Calendar.MINUTE, 1);
        }
        return dateNext;
    }

    public final boolean isReady(final Date date, final TimeZone tz) {
        final Calendar calendar = Calendar.getInstance(tz);
        calendar.setTime(date);
        return isTime(calendar);
    }

    private boolean isTime(final Calendar calendar) {
        final boolean isMinute = minute.get(calendar.get(Calendar.MINUTE));
        final boolean isHour = hour.get(calendar.get(Calendar.HOUR_OF_DAY));
        final boolean isDay = day.get(calendar.get(Calendar.DAY_OF_MONTH) - 1);  // 1 base to 0 base
        final boolean isMonth = month.get(calendar.get(Calendar.MONTH));  // 0 base to 0 base;
        final boolean isDayOfWeek = dayOfWeek.get(
                (calendar.get(Calendar.DAY_OF_WEEK) - 1) % Const.DAYS_IN_WEEK);  // sunday: (0 or 7)
        return (isMinute && isHour && isDay && isMonth && isDayOfWeek);
    }

    public static class Const {
        public static final int MINUTES_IN_HOUR = 60;
        public static final int HOURS_IN_DAY = 24;
        public static final int MAX_DAYS_IN_MONTH = 31;
        public static final int MONTHS_IN_YEAR = 12;

        public static final int DAYS_IN_WEEK = 7;
    }
}
