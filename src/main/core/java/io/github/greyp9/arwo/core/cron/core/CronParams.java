package io.github.greyp9.arwo.core.cron.core;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.table.type.RowTyped;
import org.w3c.dom.Element;

import java.io.File;
import java.security.Principal;
import java.util.Date;

public class CronParams {
    private final String context;
    private final String authorization;
    private final Principal principal;
    private final Date date;
    private final CronTab cronTab;
    private final CronJob cronJob;
    private final Element element;
    private final RowTyped row;

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
        return DateU.copy(date);
    }

    public final CronTab getCronTab() {
        return cronTab;
    }

    public final CronJob getCronJob() {
        return cronJob;
    }

    public final Element getElement() {
        return element;
    }

    public final RowTyped getRow() {
        return row;
    }

    public final File getFile(final File folder, final String filename) {
        final File folderTab = new File(folder, cronTab.getName());
        final File folderJob = new File(folderTab, cronJob.getName());
        final String dateString = DateX.toFilename(date);
        final String filenameCron = ((filename == null)
                ? String.format("cron-%s.txt", dateString)
                : String.format("cron-%s.%s", dateString, filename));
        return new File(folderJob, filenameCron);
    }

    @SuppressWarnings({ "PMD.ExcessiveParameterList", "checkstyle:parameternumber" })
    public CronParams(final String context, final String authorization, final Principal principal, final Date date,
                      final CronTab cronTab, final CronJob cronJob, final Element element, final RowTyped row) {
        this.context = context;
        this.authorization = authorization;
        this.principal = principal;
        this.date = DateU.copy(date);
        this.cronTab = cronTab;
        this.cronJob = cronJob;
        this.element = element;
        this.row = row;
    }
}
