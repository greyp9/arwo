package io.github.greyp9.arwo.core.cron.exec;

import io.github.greyp9.arwo.core.cron.core.CronParams;
import io.github.greyp9.arwo.core.cron.core.CronProperties;
import io.github.greyp9.arwo.core.cron.core.CronRunnable;
import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.tab.CronTab;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.row.Row;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.type.RowTyped;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import javax.xml.XMLConstants;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.security.Principal;
import java.util.Collection;
import java.util.Date;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

public class CronTabWork {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final String context;
    private final CronTabExecutor executor;
    private final Principal invokerPrincipal;
    private final Date dateSchedule;

    public CronTabWork(final String context, final CronTabExecutor executor,
                       final Principal principal, final Date dateSchedule) {
        this.context = context;
        this.executor = executor;
        this.invokerPrincipal = principal;
        this.dateSchedule = DateU.copy(dateSchedule);
    }

    public final void doWork(final CronJob cronJob) throws IOException {
        runJob(cronJob);
    }

    public final void doWork() throws IOException {
        final CronTab cronTab = executor.getCronTab();
        final Collection<CronJob> cronJobs = cronTab.getJobsReady(dateSchedule);
        for (final CronJob cronJob : cronJobs) {
            runJob(cronJob);
        }
    }

    private void runJob(final CronJob cronJob) throws IOException {
        final Element elementJob = cronJob.getElement();
        final Element element = ((elementJob == null) ? fabricateElement(cronJob) : elementJob);
        final CronJob cronJob1 = new CronJob(cronJob.getName(), cronJob.isEnabled(), cronJob.getLine(), element);
        final Properties properties = CronProperties.getProperties();
        final String className = properties.getProperty(element.getLocalName());
        if (className != null) {
            runJob(cronJob1, className);
        }
    }

    private Element fabricateElement(final CronJob cronJob) throws IOException {
        final String command = cronJob.getCommand();
        final String[] tokens = command.split(StringU.Const.WHITESPACE);
        final NameTypeValues arguments = HttpArguments.toArguments(tokens[1]);
        final Element element = DocumentU.createDocument(tokens[0], XMLConstants.NULL_NS_URI).getDocumentElement();
        for (final NameTypeValue argument : arguments) {
            ElementU.setAttribute(element, argument.getName(), argument.getValueS());
        }
        return element;
    }

    private void runJob(final CronJob cronJob, final String className) {
        final CronTab cronTab = executor.getCronTab();
        final ExecutorService executorService = executor.getExecutorService();
        final CronParams params = getParams(dateSchedule, cronJob);
        final CronRunnable runnable = getRunnable(className, params);
        if (runnable != null) {
            logger.info(String.format("execute(%s/%s)", cronTab.getName(), cronJob.getName()));
            executorService.execute(runnable);
        }
    }

    private CronParams getParams(final Date date, final CronJob cronJob) {
        final String authorization = executor.getAuthorization();
        final Principal principal = executor.getPrincipal();
        final CronTab cronTab = executor.getCronTab();
        final Element element = cronJob.getElement();
        final RowTyped row = createRow(date, cronJob);
        return new CronParams(context, authorization, principal, date, cronTab, cronJob, element, row);
    }

    private RowTyped createRow(final Date date, final CronJob cronJob) {
        final RowSet rowSet = executor.getRowSet();
        final Row row = new InsertRow(rowSet).getRow();
        rowSet.add(row);
        final RowTyped rowTyped = new RowTyped(rowSet.getMetaData(), row);
        rowTyped.update("cronTab", executor.getCronTab().getName());
        rowTyped.update("cronJob", cronJob.getName());
        rowTyped.update("command", cronJob.getLine());
        rowTyped.update("principal", ((invokerPrincipal == null) ? null : invokerPrincipal.getName()));
        rowTyped.update("date", date);
        return rowTyped;
    }

    @SuppressWarnings({ "PMD.OnlyOneReturn", "PMD.AvoidCatchingThrowable" })
    private CronRunnable getRunnable(final String className, final CronParams params) {
        try {
            final Class<?> c = Class.forName(className);
            final Constructor<?> ctor = c.getConstructor(CronParams.class);
            return (CronRunnable) ctor.newInstance(params);
        } catch (Throwable e) {
            logger.warning(Value.join(Http.Token.SLASH, e.getClass().getSimpleName(), e.getMessage()));
            return null;
        }
    }
}
