package io.github.greyp9.arwo.core.cron.job.factory;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.job.CronToken;
import io.github.greyp9.arwo.core.lang.StringU;

import java.util.BitSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CronJobFactory {

    public final CronJob create(final String line) {
        final Matcher matcher = Pattern.compile(Const.PATTERN_LINE).matcher(line);
        return (matcher.matches() ? create(line, matcher) : null);
    }

    private CronJob create(final String line, final Matcher matcher) {
        int group = 0;
        final BitSet minute = new CronToken(CronJob.Const.MINUTES_IN_HOUR, 0).apply(matcher.group(++group));
        final BitSet hour = new CronToken(CronJob.Const.HOURS_IN_DAY, 0).apply(matcher.group(++group));
        final BitSet day = new CronToken(CronJob.Const.MAX_DAYS_IN_MONTH, -1).apply(matcher.group(++group));
        final BitSet month = new CronToken(CronJob.Const.MONTHS_IN_YEAR, -1).apply(matcher.group(++group));
        final BitSet dayOfWeek = new CronToken(CronJob.Const.DAYS_IN_WEEK, 0).apply(matcher.group(++group));
        final String command = matcher.group(++group);
        return new CronJob(line, minute, hour, day, month, dayOfWeek, command);
    }

    private static class Const {
        private static final int CRON_ENTRY_TOKENS = 5;

        private static final String PATTERN_TOKEN = "(\\S+)\\s+";
        private static final String PATTERN_LINE = StringU.create(CRON_ENTRY_TOKENS, PATTERN_TOKEN) + "(.*)";
    }
}
