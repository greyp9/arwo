package io.github.greyp9.arwo.core.security.update;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.security.realm.AppRealm;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.trigger.XedTrigger;

import javax.naming.Context;
import java.util.concurrent.ExecutorService;

public class RealmTrigger implements XedTrigger {

    @Override
    public final void onPersist(final String contextPath, final Xed xed) {
        final Context context = AppNaming.lookupSubcontext(contextPath);
        final AppRealm appRealm = AppRealmFactory.toAppRealm(contextPath);
        final ExecutorService executor = (ExecutorService)
                AppNaming.lookup(contextPath, App.Naming.EXECUTOR_SERVICE);
        final RealmUpdateRunnable updateRunnable = new RealmUpdateRunnable(context, appRealm);
        executor.execute(updateRunnable);
    }
}
