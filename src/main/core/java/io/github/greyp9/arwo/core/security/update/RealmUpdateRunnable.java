package io.github.greyp9.arwo.core.security.update;

import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.security.realm.AppRealm;
import io.github.greyp9.arwo.core.security.realm.AppRealmContainer;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import javax.naming.Context;
import java.util.logging.Logger;

@SuppressWarnings("PMD.DoNotUseThreads")
public class RealmUpdateRunnable implements Runnable {
    private final Logger logger;
    private final Context context;
    private final AppRealm appRealm;
    private boolean cancelled;

    public RealmUpdateRunnable(final Context context, final AppRealm appRealm) {
        this.logger = Logger.getLogger(getClass().getName());
        this.context = context;
        this.appRealm = appRealm;
        this.cancelled = false;
    }

    public final void cancel() {
        cancelled = true;
    }

    @SuppressWarnings({ "PMD.GuardLogStatementJavaUtil", "PMD.GuardLogStatement" })
    public final void run() {
        logger.info("RUN/" + appRealm.getSize());  // i18n log
        // wait until AppRealmContainer registered (in Tomcat, on background thread)
        boolean interrupted = false;
        Object o = AppNaming.lookup(context, AppRealmContainer.NAMING_CONTAINER);
        while ((!interrupted) && (!cancelled) && (o == null)) {
            interrupted = ThreadU.sleepMillis(DateU.Const.ONE_SECOND_MILLIS);
            o = AppNaming.lookup(context, AppRealmContainer.NAMING_CONTAINER);
        }
        // update realm
        if (o instanceof AppRealmContainer) {
            final AppRealmContainer appRealmContainer = (AppRealmContainer) o;
            appRealmContainer.setAppRealm(appRealm);
            logger.info("OK");  // i18n log
        } else if (interrupted) {
            logger.info("INTERRUPTED");  // i18n log
        } else if (cancelled) {
            logger.info("CANCEL");  // i18n log
        } else {
            logger.warning("ERR/" + ((o == null) ? "" : o.getClass().getName()));  // i18n log
        }
    }
}
