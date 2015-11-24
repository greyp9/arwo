package io.github.greyp9.arwo.core.xed.session;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.trigger.XedTrigger;

import java.io.File;
import java.util.Date;

public class XedSession {
    private final XedEntry entry;
    private final Xed xed;
    private final File file;
    private final Date dateLoad;
    private final XedTrigger trigger;

    private Date dateModify;

    public final XedEntry getEntry() {
        return entry;
    }

    public final Xed getXed() {
        return xed;
    }

    public final File getFile() {
        return file;
    }

    public final Date getDateLoad() {
        return DateU.copy(dateLoad);
    }

    public final XedTrigger getTrigger() {
        return trigger;
    }

    public final Date getDateModify() {
        return DateU.copy(dateModify);
    }

    public final void setDateModify(final Date dateModify) {
        this.dateModify = DateU.copy(dateModify);
    }

    public XedSession(final XedEntry entry, final Xed xed,
                      final File file, final Date dateLoad, final XedTrigger trigger) {
        this.entry = entry;
        this.xed = xed;
        this.file = file;
        this.dateLoad = DateU.copy(dateLoad);
        this.trigger = trigger;
    }
}
