package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.text.TextFilters;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.Date;

public class AppUserState {
    private final Principal principal;
    private final Interval interval;
    private final File userHome;
    private final String submitID;
    private final ViewStates viewStates;
    private final TextFilters textFilters;
    private final Locus locus;
    private final Alerts alerts;
    private final XedUserState documentState;
    // connection entries
    private final ConnectionCache cacheSSH;

    public final Principal getPrincipal() {
        return principal;
    }

    public final Interval getInterval() {
        return interval;
    }

    public final File getUserHome() {
        return userHome;
    }

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final TextFilters getTextFilters() {
        return textFilters;
    }

    public final Locus getLocus() {
        return locus;
    }

    public final Alerts getAlerts() {
        return alerts;
    }

    public final XedUserState getDocumentState() {
        return documentState;
    }

    public final ConnectionCache getCacheSSH() {
        return cacheSSH;
    }

    public AppUserState(final Principal principal, final Date date, final File webappRoot,
                        final String submitID, final Locus locus) throws IOException {
        this.principal = principal;
        this.interval = new Interval(date, null);
        this.userHome = AppFolder.getUserHome(webappRoot, principal);
        this.submitID = submitID;
        this.viewStates = new ViewStates();
        this.textFilters = new TextFilters();
        this.locus = locus;
        this.alerts = new Alerts();
        this.documentState = new XedUserState(webappRoot, principal, submitID, locus, alerts);
        this.cacheSSH = new ConnectionCache("ssh", alerts);
    }
}
