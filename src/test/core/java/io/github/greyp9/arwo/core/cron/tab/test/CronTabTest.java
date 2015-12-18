package io.github.greyp9.arwo.core.cron.tab.test;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.job.factory.CronJobFactory;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.XsdDateU;
import junit.framework.TestCase;
import org.junit.Assert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

public class CronTabTest extends TestCase {

    public void testTabFrequent() throws Exception {
        final CronJobFactory factory = new CronJobFactory();
        final Collection<CronJob> jobs = new ArrayList<CronJob>();
        jobs.add(factory.create("every-minute", "* * * * * ls"));
        jobs.add(factory.create("every-hour", "0 * * * * ls"));
        final CronTab cronTab = new CronTab("couple-of-jobs", jobs, DateU.Const.TZ_GMT);
        Assert.assertEquals("couple-of-jobs", cronTab.getName());
        Assert.assertEquals(jobs, cronTab.getJobs());
        Assert.assertEquals(DateU.Const.TZ_GMT, cronTab.getTZ());
        final Date dateThis = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        Assert.assertEquals(2, cronTab.getJobsReady(dateThis).size());
        final Date dateNext = cronTab.getDateNext(dateThis, "PT1H");
        Assert.assertEquals("2000-01-01T00:01:00Z", XsdDateU.toXSDZ(dateNext));
        Assert.assertEquals(1, cronTab.getJobsReady(dateNext).size());
    }

    public void testTabInfrequent() throws Exception {
        final CronJobFactory factory = new CronJobFactory();
        final Collection<CronJob> jobs = new ArrayList<CronJob>();
        jobs.add(factory.create("every-day", "0 12 * * * ls"));
        jobs.add(factory.create("every-week", "0 12 * * 1 ls"));
        final CronTab cronTab = new CronTab("couple-of-jobs", jobs, DateU.Const.TZ_GMT);
        Assert.assertEquals("couple-of-jobs", cronTab.getName());
        Assert.assertEquals(jobs, cronTab.getJobs());
        Assert.assertEquals(DateU.Const.TZ_GMT, cronTab.getTZ());
        final Date dateThis = XsdDateU.fromXSDZ("2000-01-01T00:00:00.000Z");
        Assert.assertEquals(0, cronTab.getJobsReady(dateThis).size());
        // short period, nothing found
        final Date dateNext = cronTab.getDateNext(dateThis, "PT1H");
        Assert.assertEquals("2000-01-01T01:00:00Z", XsdDateU.toXSDZ(dateNext));
        Assert.assertEquals(0, cronTab.getJobsReady(dateNext).size());
        // longer period, found a job
        final Date dateNext2 = cronTab.getDateNext(dateNext, "P1D");
        Assert.assertEquals("2000-01-01T12:00:00Z", XsdDateU.toXSDZ(dateNext2));
        Assert.assertEquals(1, cronTab.getJobsReady(dateNext2).size());
    }
}
