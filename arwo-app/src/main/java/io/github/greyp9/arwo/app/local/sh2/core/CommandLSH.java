package io.github.greyp9.arwo.app.local.sh2.core;

import java.util.Date;

public final class CommandLSH {
    private final Date date;
    private final String in;
    private Date dateFinish;
    private String out;

    public CommandLSH(final Date date, final String in) {
        this.date = date;
        this.in = in;
    }

    public Date getDate() {
        return date;
    }

    public String getIn() {
        return in;
    }

    public Date getDateFinish() {
        return dateFinish;
    }

    public void setDateFinish(final Date dateFinish) {
        this.dateFinish = dateFinish;
    }

    public String getOut() {
        return out;
    }

    public void setOut(final String out) {
        this.out = out;
    }
}
