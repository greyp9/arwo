package io.github.greyp9.arwo.core.time;

import java.util.Date;

public final class Timer {
    private final long interval;
    private Date dateStart;

    public Timer(final long interval) {
        this.interval = interval;
        this.dateStart = new Date();
    }

    public boolean check() {
        final Date dateNow = new Date();
        final boolean alert = ((dateNow.getTime() - dateStart.getTime()) >= interval);
        if (alert) {
            dateStart = dateNow;
        }
        return alert;
    }
}
