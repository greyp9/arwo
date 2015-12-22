package io.github.greyp9.arwo.core.cron.service;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cron.exec.CronTabExecutor;
import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.exec.ExecutorServiceFactory;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Element;

import java.io.IOException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.concurrent.ExecutorService;

public class CronServiceRegistrar {
    private final Date date;
    private final String authorization;
    private final Principal principal;
    private final Bundle bundle;
    private final Alerts alerts;
    private final CronService cronService;

    public CronServiceRegistrar(final Date date, final String authorization, final Principal principal,
                                final Bundle bundle, final Alerts alerts, final CronService cronService) {
        this.date = DateU.copy(date);
        this.authorization = authorization;
        this.principal = principal;
        this.bundle = bundle;
        this.alerts = alerts;
        this.cronService = cronService;
    }

    public final void unregister() throws IOException {
        cronService.remove(principal.getName(), date, bundle, alerts);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final void register(final Xed xed) throws IOException {
        final XPather xpather = xed.getXPather();
        final List<Element> elementsCronTab = xpather.getElements("/app:app/app:cron/app:cronTab[@enabled='true']");
        for (final Element elementCronTab : elementsCronTab) {
            final CronTabExecutor executor = addTab(new XPather(elementCronTab, xpather.getContext()));
            cronService.add(executor, bundle, alerts);
        }
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private CronTabExecutor addTab(final XPather xpatherTab) throws IOException {
        final String name = xpatherTab.getText("@name");
        final String threads = xpatherTab.getText("@threads");
        final String tz = xpatherTab.getText("@tz");
        final TimeZone timeZone = TimeZone.getTimeZone(tz);
        final Collection<CronJob> cronJobs = new ArrayList<CronJob>();
        final CronTab cronTab = new CronTab(name, cronJobs, timeZone);
        final List<Element> elementsJob = xpatherTab.getElements("app:cronJob[@enabled='true']");
        for (final Element elementJob : elementsJob) {
            cronJobs.add(createJob(new XPather(elementJob, xpatherTab.getContext())));
        }
        final Integer nThreads = NumberU.toInt(threads, 0);
        final String prefix = String.format("CRON-TAB-%s", name);
        final ExecutorService executorService = ExecutorServiceFactory.create(nThreads, prefix);
        return new CronTabExecutor(authorization, principal, executorService, cronTab, date);
    }

    private CronJob createJob(final XPather xpatherJob) throws IOException {
        final String name = xpatherJob.getText("@name");
        final String cron = xpatherJob.getText("@cron");
        final Element jobType = xpatherJob.getElement("*");
        final String cron1 = ((jobType == null) ? cron : Value.join(" ", cron, jobType.getLocalName()));
        return new CronJob(name, true, cron1, jobType);
    }
}
