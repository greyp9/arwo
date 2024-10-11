package io.github.greyp9.arwo.core.date;

import java.util.Date;

public class Interval {
    private Date dateStart;
    private Date dateFinish;

    public Interval(final Date dateStart, final Date dateFinish) {
        this.dateStart = DateU.copy(dateStart);
        this.dateFinish = DateU.copy(dateFinish);
    }

    public final Date getDateStart() {
        return DateU.copy(dateStart);
    }

    public final Date getDateFinish() {
        return DateU.copy(dateFinish);
    }

    public final void setDateStart(final Date dateStart) {
        this.dateStart = DateU.copy(dateStart);
    }

    public final void setDateFinish(final Date dateFinish) {
        this.dateFinish = DateU.copy(dateFinish);
    }
}
