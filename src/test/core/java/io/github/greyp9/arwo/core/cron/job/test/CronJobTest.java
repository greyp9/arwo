package io.github.greyp9.arwo.core.cron.job.test;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.date.XsdDateU;
import junit.framework.TestCase;
import org.junit.Assert;

import java.util.Date;
import java.util.TimeZone;

public class CronJobTest extends TestCase {

    public void testJob() throws Exception {
        final CronJob cronJob = new CronJob(null, true, "* * * * * ping localhost", null);
        Assert.assertNotNull(cronJob);
        Assert.assertEquals("* * * * * ping localhost", cronJob.getLine());
        Assert.assertEquals("ping localhost", cronJob.getCommand());
    }

    public void testMatrix() throws Exception {
        for (final String[] row : Const.MATRIX) {
            // setup
            int i = -1;
            final String label = row[++i];
            final String line = row[++i];
            final TimeZone tz = TimeZone.getTimeZone(row[++i]);
            final Date dateCheck = XsdDateU.fromXSDZ(row[++i]);
            final String durationCheck = row[++i];
            final boolean isReadyExpected = Boolean.parseBoolean(row[++i]);
            final Date dateNextExpected = XsdDateU.fromXSDZ(row[++i]);
            // test
            final CronJob cronJob = new CronJob(null, true, line, null);
            Assert.assertNotNull(label, cronJob);
            Assert.assertEquals(label, isReadyExpected, cronJob.isReady(dateCheck, tz));
            Assert.assertEquals(label, dateNextExpected, cronJob.getDateNext(dateCheck, tz, durationCheck));
        }
    }

    private static class Const {
        private static final String TZ_GMT = "GMT";
        private static final String TZ_US = "America/New_York";
        private static final String TZ_JP = "Asia/Tokyo";

        // cronText, tz, dateCheck, durationCheck, isReady, dateNext
        private static final String[][] MATRIX = new String[][] {
                { "a", "* * * * * ls", TZ_GMT, "2000-01-01T00:00:00.000Z", "PT1H", "true", "2000-01-01T00:01:00.000Z" },

                { "ba", "* * * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "PT1H", "true", "2000-01-01T00:01:00Z" },
                { "bb", "* * * * * ls", TZ_GMT, "2000-01-01T00:01:00Z", "PT1H", "true", "2000-01-01T00:02:00Z" },
                { "bc", "0 * * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "PT1H", "true", "2000-01-01T01:00:00Z" },
                { "bd", "0 * * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "PT10M", "true", null },

                { "ca", "0,10,20 * * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "PT1H", "true", "2000-01-01T00:10:00Z" },
                { "cb", "0,10,20 * * * * ls", TZ_GMT, "2000-01-01T00:01:00Z", "PT1H", "false", "2000-01-01T00:10:00Z" },
                { "cc", "0,10-20 * * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "PT1H", "true", "2000-01-01T00:10:00Z" },
                { "cd", "0,10-20 * * * * ls", TZ_GMT, "2000-01-01T00:01:00Z", "PT1H", "false", "2000-01-01T00:10:00Z" },
                { "ce", "*/5 * * * * ls", TZ_GMT, "2000-01-01T00:05:00Z", "PT1H", "true", "2000-01-01T00:10:00Z" },
                { "cf", "*/5 * * * * ls", TZ_GMT, "2000-01-01T00:06:00Z", "PT1H", "false", "2000-01-01T00:10:00Z" },

                { "da", "0 0 * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "PT1H", "true", null },
                { "db", "1 0 * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "PT1H", "false", "2000-01-01T00:01:00Z" },
                { "dc", "0 0 * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "P1D", "true", "2000-01-02T00:00:00Z" },
                { "dd", "1 0 * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "P1D", "false", "2000-01-01T00:01:00Z" },
                { "de", "1 0 1 * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "P1D", "false", "2000-01-01T00:01:00Z" },
                { "df", "1 0 1 * * ls", TZ_GMT, "2000-01-01T00:01:00Z", "P1D", "true", null },
                { "dg", "1 0 1 1 * ls", TZ_GMT, "2000-01-01T00:00:00Z", "P1D", "false", "2000-01-01T00:01:00Z" },
                { "dh", "1 0 1 1 * ls", TZ_GMT, "2000-01-01T00:01:00Z", "P1D", "true", null },

                { "ea", "0 0 * * 0 ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-02T00:00:00Z" },
                { "eb", "0 0 * * 1 ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-03T00:00:00Z" },
                { "ec", "0 0 * * 2 ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-04T00:00:00Z" },
                { "ed", "0 0 * * 3 ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-05T00:00:00Z" },
                { "ee", "0 0 * * 4 ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-06T00:00:00Z" },
                { "ef", "0 0 * * 5 ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-07T00:00:00Z" },
                { "eg", "0 0 * * 6 ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "true", "2000-01-08T00:00:00Z" },
                { "eh", "0 0 * * 7 ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-02T00:00:00Z" },

                { "fa", "0 * * * * ls", TZ_GMT, "2000-01-01T00:59:00Z", "PT1H", "false", "2000-01-01T01:00:00Z" },
                { "fb", "59 * * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "PT1H", "false", "2000-01-01T00:59:00Z" },
                { "fc", "0 0 * * * ls", TZ_GMT, "2000-01-01T23:59:00Z", "P1D", "false", "2000-01-02T00:00:00Z" },
                { "fd", "59 23 * * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "P1D", "false", "2000-01-01T23:59:00Z" },
                { "fe", "0 0 2 * * ls", TZ_GMT, "2000-01-01T23:59:00Z", "P7D", "false", "2000-01-02T00:00:00Z" },
                { "ff", "59 23 1 * * ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-01T23:59:00Z" },
                { "fg", "0 0 1 2 * ls", TZ_GMT, "2000-01-31T23:59:00Z", "P7D", "false", "2000-02-01T00:00:00Z" },
                { "fh", "59 23 1 1 * ls", TZ_GMT, "2000-01-01T00:00:00Z", "P7D", "false", "2000-01-01T23:59:00Z" },

                { "ga", "0 0 * * * dir", TZ_US, "2000-01-01T00:00:00Z", "P1D", "false", "2000-01-01T05:00:00Z" },
                { "gb", "0 0 * * * dir", TZ_JP, "2000-01-01T00:00:00Z", "P1D", "false", "2000-01-01T15:00:00Z" },
        };
    }
}
