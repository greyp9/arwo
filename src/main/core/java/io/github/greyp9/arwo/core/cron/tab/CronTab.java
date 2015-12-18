package io.github.greyp9.arwo.core.cron.tab;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.date.DurationU;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.TimeZone;

public class CronTab {
    private final String name;
    private final Collection<CronJob> jobs;
    private final TimeZone tz;

    public final String getName() {
        return name;
    }

    public final Collection<CronJob> getJobs() {
        return jobs;
    }

    public final TimeZone getTZ() {
        return tz;
    }

    public CronTab(final String name, final Collection<CronJob> jobs, final TimeZone tz) {
        this.name = name;
        this.jobs = jobs;
        this.tz = tz;
    }

    public final Collection<CronJob> getJobsReady(final Date date) {
        final Collection<CronJob> jobsReady = new ArrayList<CronJob>();
        for (final CronJob job : jobs) {
            if (job.isReady(date, tz)) {
                jobsReady.add(job);
            }
        }
        return jobsReady;
    }

    public final Date getDateNext(final Date date, final String duration) {
        Date dateNext = DurationU.add(date, tz, duration);
        for (final CronJob job : jobs) {
            final Date dateNextJob = job.getDateNext(date, tz, duration);
            final boolean isEarlier = ((dateNextJob != null) && (dateNextJob.before(dateNext)));
            dateNext = (isEarlier ? dateNextJob : dateNext);
        }
        return dateNext;
    }
}
