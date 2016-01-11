package io.github.greyp9.arwo.core.io.command;

import java.util.Date;

@SuppressWarnings("PMD.AbstractNaming")
public abstract class Command {

    public abstract String getDir();

    public abstract String getStdin();

    public abstract String getStdout();

    public abstract String getStderr();

    public abstract Date getStart();

    public abstract Date getFinish();

    public abstract Integer getPID();

    public abstract Integer getExitValue();

    public final Long getElapsed(final Date date) {
        final Date dateA = getStart();
        final Date dateFinish = getFinish();
        final Date dateZ = ((dateFinish == null) ? date : dateFinish);
        final boolean isValue = ((dateA != null) && (dateZ != null));
        return (isValue ? (dateZ.getTime() - dateA.getTime()) : null);
    }
}
