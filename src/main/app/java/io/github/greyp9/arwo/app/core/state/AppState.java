package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.locus.Locus;

import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

public class AppState {
    private final String contextPath;
    private final Collection<AppUserState> userStates;

    public AppState(final String contextPath) {
        this.contextPath = contextPath;
        this.userStates = new ArrayList<AppUserState>();
    }

    public final AppUserState getUserState(final Principal principal, final Date date) throws IOException {
        synchronized (userStates) {
            AppUserState userState = find(principal);
            if (userState == null) {
                final File userHome = AppFolder.getWebappRoot(contextPath);
                final String submitID = Integer.toHexString(hashCode());
                final DateX dateX = new DateX(HttpDateU.Const.DEFAULT, TimeZone.getTimeZone("UTC"));
                final Locus locus = new Locus(Locale.getDefault(), dateX);
                userState = new AppUserState(principal, date, userHome, submitID, locus);
                userStates.add(userState);
            }
            return userState;
        }
    }

/*
    public synchronized void removeUserState(final Principal principal, final Date date) throws IOException {
        AppUserState userState = find(principal);
        if (userState != null) {
            userStates.remove(userState);
        }
    }
*/

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
