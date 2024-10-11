package io.github.greyp9.arwo.app.core.subsystem.cron;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.meter.Meter;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.xed.model.XedFactory;

import java.io.IOException;

public class SubsystemCron {
    private final Meter meterCron;
    private final RowSet rowSetCron;

    public final Meter getMeterCron() {
        return meterCron;
    }

    public final RowSet getRowSetCron() {
        return rowSetCron;
    }

    public SubsystemCron(final XedFactory factory) throws IOException {
        this.meterCron = new Meter(App.Meter.QNAME_CRON_JOBS, factory);
        this.rowSetCron = this.meterCron.getRowSet("{urn:arwo:meter}cronJobsType", "job");  // i18n
    }
}
