package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.cron.service.CronService;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.locus.Locus;

import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Locale;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicReference;

public class AppState {
    private final Date dateStart;
    private final String contextPath;
    private final CronService cronService;
    private final Collection<AppUserState> userStates;
    private final AtomicReference<String> reference;

    public final Date getDateStart() {
        return DateU.copy(dateStart);
    }

    public final String getContextPath() {
        return contextPath;
    }

    public final CronService getCronService() {
        return cronService;
    }

    public final AtomicReference<String> getReference() {
        return reference;
    }

    public AppState(final String contextPath) {
        this.dateStart = new Date();
        this.contextPath = contextPath;
        this.cronService = new CronService(contextPath);
        this.userStates = new ArrayList<AppUserState>();
        this.reference = new AtomicReference<String>();
    }

    public final Iterator<AppUserState> getIterator() {
        return userStates.iterator();
    }

    public final void shutdown(final String cause) {
        reference.set(cause);
    }

    public final AppUserState getUserState(final Principal principal, final Date date) throws IOException {
        synchronized (userStates) {
            AppUserState userState = find(principal);
            if (userState == null) {
                final File userHome = AppFolder.getWebappRoot(contextPath);
                final String submitID = Integer.toHexString(hashCode());
                final DateX dateX = new DateX(HttpDateU.Const.DEFAULT, TimeZone.getTimeZone("UTC"));
                final Locus locus = new Locus(Locale.getDefault(), dateX);
                userState = new AppUserState(this, principal, date, userHome, submitID, locus);
                userStates.add(userState);
            }
            return userState;
        }
    }

    public final void removeUserState(final Principal principal, final Date date) throws IOException {
        synchronized (userStates) {
            final AppUserState userState = find(principal);
            if (userState != null) {
                //userStates.remove(userState);
                userState.close(date);
            }
        }
    }

    private AppUserState find(final Principal principal) {
        AppUserState userState = null;
        for (final AppUserState userStateIt : userStates) {
            final boolean isMatchPrincipal = userStateIt.getPrincipal().getName().equals(principal.getName());
            final boolean isActive = (userStateIt.getInterval().getDateFinish() == null);
            if (isMatchPrincipal && isActive) {
                userState = userStateIt;
            }
        }
        return userState;
    }
}
