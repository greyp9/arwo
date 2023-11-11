package io.github.greyp9.arwo.core.cron.tab.test;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

public class CronTabTest {

    @Test
    public void testTabFrequent() throws Exception {
        final Collection<CronJob> jobs = new ArrayList<CronJob>();
        jobs.add(new CronJob("every-minute", true, "* * * * * ls", null));
        jobs.add(new CronJob("every-hour", true, "0 * * * * ls", null));
        final CronTab cronTab = new CronTab("couple-of-jobs", jobs, DateU.Const.TZ_GMT);
        Assertions.assertEquals("couple-of-jobs", cronTab.getName());
        Assertions.assertEquals(jobs, cronTab.getJobs());
        Assertions.assertEquals(DateU.Const.TZ_GMT, cronTab.getTZ());
        final Date dateThis = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        Assertions.assertEquals(2, cronTab.getJobsReady(dateThis).size());
        final Date dateNext = cronTab.getDateNext(dateThis, "PT1H");
        Assertions.assertEquals("2000-01-01T00:01:00Z", XsdDateU.toXSDZ(dateNext));
        Assertions.assertEquals(1, cronTab.getJobsReady(dateNext).size());
    }

    @Test
    public void testTabInfrequent() throws Exception {
        final Collection<CronJob> jobs = new ArrayList<CronJob>();
        jobs.add(new CronJob("every-day", true, "0 12 * * * ls", null));
        jobs.add(new CronJob("every-week", true, "0 12 * * 1 ls", null));
        final CronTab cronTab = new CronTab("couple-of-jobs", jobs, DateU.Const.TZ_GMT);
        Assertions.assertEquals("couple-of-jobs", cronTab.getName());
        Assertions.assertEquals(jobs, cronTab.getJobs());
        Assertions.assertEquals(DateU.Const.TZ_GMT, cronTab.getTZ());
        final Date dateThis = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        Assertions.assertEquals(0, cronTab.getJobsReady(dateThis).size());
        // short period, nothing found
        final Date dateNext = cronTab.getDateNext(dateThis, "PT1H");
        Assertions.assertEquals("2000-01-01T01:00:00Z", XsdDateU.toXSDZ(dateNext));
        Assertions.assertEquals(0, cronTab.getJobsReady(dateNext).size());
        // longer period, found a job
        final Date dateNext2 = cronTab.getDateNext(dateNext, "P1D");
        Assertions.assertEquals("2000-01-01T12:00:00Z", XsdDateU.toXSDZ(dateNext2));
        Assertions.assertEquals(1, cronTab.getJobsReady(dateNext2).size());
    }
}
