package io.github.greyp9.arwo.core.cron.core;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;

import java.io.File;
import java.security.Principal;
import java.util.Date;

public class CronParams {
    private final String context;
    private final String authorization;
    private final Principal principal;
    private final Date dateSchedule;
    private final CronTab cronTab;
    private final CronJob cronJob;

    public final String getContext() {
        return context;
    }

    public final String getAuthorization() {
        return authorization;
    }

    public final Principal getPrincipal() {
        return principal;
    }

    public final Date getDate() {
        return DateU.copy(dateSchedule);
    }

    public final CronTab getCronTab() {
        return cronTab;
    }

    public final CronJob getCronJob() {
        return cronJob;
    }

    public final File getFile(final File folder) {
        final File folderTab = new File(folder, cronTab.getName());
        final File folderJob = new File(folderTab, cronJob.getName());
        return new File(folderJob, String.format("cron-%s.txt", DateX.toFilename(dateSchedule)));
    }

    public CronParams(final String context, final String authorization, final Principal principal,
                      final Date date, final CronTab cronTab, final CronJob cronJob) {
        this.context = context;
        this.authorization = authorization;
        this.principal = principal;
        this.dateSchedule = DateU.copy(date);
        this.cronTab = cronTab;
        this.cronJob = cronJob;
    }
}
