package io.github.greyp9.arwo.core.cron.job;

import io.github.greyp9.arwo.core.date.DurationU;
import org.w3c.dom.Element;

import java.util.BitSet;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

public class CronJob {
    // config
    private final String name;
    private final boolean enabled;
    private final String line;
    private final Element element;
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

    public final boolean isEnabled() {
        return enabled;
    }

    public final String getLine() {
        return line;
    }

    public final Element getElement() {
        return element;
    }

    public final String getCommand() {
        return command;
    }

    public CronJob(final String name, final boolean enabled, final String line, final Element element) {
        this.name = name;
        this.enabled = enabled;
        this.line = line;
        this.element = element;
        final CronLine cronLine = new CronLine(line);
        this.minute = cronLine.getMinute();
        this.hour = cronLine.getHour();
        this.day = cronLine.getDay();
        this.month = cronLine.getMonth();
        this.dayOfWeek = cronLine.getDayOfWeek();
        this.command = cronLine.getCommand();
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
            if (isReady(calendar)) {
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
        return isReady(calendar);
    }

    private boolean isReady(final Calendar calendar) {
        final boolean isMinute = minute.get(calendar.get(Calendar.MINUTE));
        final boolean isHour = hour.get(calendar.get(Calendar.HOUR_OF_DAY));
        final boolean isDay = day.get(calendar.get(Calendar.DAY_OF_MONTH) - 1);  // 1 base to 0 base
        final boolean isMonth = month.get(calendar.get(Calendar.MONTH));  // 0 base to 0 base;
        final boolean isDayOfWeek = dayOfWeek.get((calendar.get(Calendar.DAY_OF_WEEK) - 1));  // sunday: (0 or 7)
        return (isMinute && isHour && isDay && isMonth && isDayOfWeek);
    }
}
