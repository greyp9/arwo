package io.github.greyp9.arwo.core.cron.job;

import io.github.greyp9.arwo.core.lang.StringU;

import java.util.BitSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CronLine {
    private final BitSet minute;
    private final BitSet hour;
    private final BitSet day;
    private final BitSet month;
    private final BitSet dayOfWeek;
    private final String command;

    public final BitSet getMinute() {
        return minute;
    }

    public final BitSet getHour() {
        return hour;
    }

    public final BitSet getDay() {
        return day;
    }

    public final BitSet getMonth() {
        return month;
    }

    public final BitSet getDayOfWeek() {
        return dayOfWeek;
    }

    public final String getCommand() {
        return command;
    }

    public CronLine(final String line) {
        final Matcher matcher = Pattern.compile(Const.PATTERN_LINE).matcher(line);
        if (matcher.matches()) {
            int group = 0;
            this.minute = new CronToken(Const.MINUTES_IN_HOUR, 0).apply(matcher.group(++group));
            this.hour = new CronToken(Const.HOURS_IN_DAY, 0).apply(matcher.group(++group));
            this.day = new CronToken(Const.MAX_DAYS_IN_MONTH, -1).apply(matcher.group(++group));
            this.month = new CronToken(Const.MONTHS_IN_YEAR, -1).apply(matcher.group(++group));
            this.dayOfWeek = new CronToken(Const.DAYS_IN_WEEK, 0).apply(matcher.group(++group));
            this.command = matcher.group(++group);
        } else {
            this.minute = new BitSet(Const.MINUTES_IN_HOUR);
            this.hour = new BitSet(Const.HOURS_IN_DAY);
            this.day = new BitSet(Const.MAX_DAYS_IN_MONTH);
            this.month = new BitSet(Const.MONTHS_IN_YEAR);
            this.dayOfWeek = new BitSet(Const.DAYS_IN_WEEK);
            this.command = null;
        }
    }

    public static class Const {
        private static final int MINUTES_IN_HOUR = 60;
        private static final int HOURS_IN_DAY = 24;
        private static final int MAX_DAYS_IN_MONTH = 31;
        private static final int MONTHS_IN_YEAR = 12;
        private static final int DAYS_IN_WEEK = 7;

        private static final int CRON_ENTRY_TOKENS = 5;

        private static final String PATTERN_TOKEN = "(\\S+)\\s+";  // i18n internal
        private static final String PATTERN_LINE = StringU.create(CRON_ENTRY_TOKENS, PATTERN_TOKEN) + "(.*)";
    }
}
