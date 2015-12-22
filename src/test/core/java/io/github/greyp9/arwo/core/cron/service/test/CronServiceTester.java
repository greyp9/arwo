package io.github.greyp9.arwo.core.cron.service.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.cron.exec.CronTabExecutor;
import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.service.CronService;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.io.runnable.InputStreamRunnable;
import io.github.greyp9.arwo.core.logging.LoggerU;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;
import io.github.greyp9.arwo.core.vm.mutex.MutexU;

import java.io.IOException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

public class CronServiceTester {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private void run() throws Exception {
        logger.info("APP/BEGIN");
        // execute
        ExecutorService executorService = ExecutorServiceFactory.create(2, getClass().getSimpleName());
        // start monitor
        ByteBuffer byteBuffer = new ByteBuffer(UTF8Codec.Const.UTF8);
        InputStreamRunnable isr = new InputStreamRunnable(System.in, byteBuffer, DurationU.toMillis("PT1S"));
        executorService.execute(isr);
        // start runner
        CronService cronService = new CronService(getClass().getSimpleName());
        executorService.execute(cronService);
        // message loop
        boolean stop = false;
        while (!stop) {
            Date dateNext = DurationU.add(new Date(), DateU.Const.TZ_GMT, "PT1M");
            stop = MutexU.waitUntil(isr, dateNext);
            stop |= process(byteBuffer.getBytes(true), cronService);
        }
        // tearDown
        cronService.stop(getClass().getSimpleName());
        isr.stop();
        List<Runnable> runnables = executorService.shutdownNow();
        for (Runnable runnable : runnables) {
            logger.warning("APP/TERMINATE" + runnable.toString());
        }
        logger.info("APP/END");
    }

    private boolean process(byte[] input, CronService cronService) throws IOException {
        Date date = new Date();
        boolean stop = false;
        String text = UTF8Codec.toString(input);
        Principal user1 = new AppPrincipal("user1", Collections.singleton("*"));
        Principal user2 = new AppPrincipal("user2", Collections.singleton("*"));
        // act on input
        if (text.equals("\n")) {
            stop = true;
        } else if (text.equals("-2\n")) {
            cronService.remove("user2", date, null, null);
        } else if (text.equals("-1\n")) {
            cronService.remove("user1", date, null, null);
        } else if (text.equals("1\n")) {
            cronService.add(getCronTabExecutor("user1", user1), null, null);
        } else if (text.equals("2\n")) {
            cronService.add(getCronTabExecutor("user2", user2), null, null);
        }
        if (input.length > 0) {
            MutexU.notifyAll(cronService);
        }
        return stop;
    }

    private CronTabExecutor getCronTabExecutor(String authorization, Principal principal) {
        ExecutorService executorService = ExecutorServiceFactory.create(1, getClass().getSimpleName());
        return new CronTabExecutor(authorization, principal, executorService, getCronTab(), new Date());
    }

    private CronTab getCronTab() {
        Collection<CronJob> cronJobs = new ArrayList<CronJob>();
        cronJobs.add(new CronJob("sleep-PT1S", true, "* * * * * sleep duration=PT1S", null));
        cronJobs.add(new CronJob("sleep-PT2S", true, "* * * * * sleep duration=PT2S", null));
        return new CronTab("tab1", cronJobs, DateU.Const.TZ_GMT);
    }

    public static void main(String[] args) {
        LoggerU.adjust(Logger.getLogger(""));
        try {
            new CronServiceTester().run();
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }
}
