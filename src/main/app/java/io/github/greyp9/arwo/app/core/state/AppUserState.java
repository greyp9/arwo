package io.github.greyp9.arwo.app.core.state;

import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.date.Interval;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewStates;
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
    private final Locus locus;
    private final XedUserState documentState;

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

    public final Locus getLocus() {
        return locus;
    }

    public final XedUserState getDocumentState() {
        return documentState;
    }

    public AppUserState(final Principal principal, final Date date, final File webappRoot,
                        final String submitID, final Locus locus) throws IOException {
        this.principal = principal;
        this.interval = new Interval(date, null);
        this.userHome = AppFolder.getUserHome(webappRoot, principal);
        this.submitID = submitID;
        this.viewStates = new ViewStates();
        this.locus = locus;
        this.documentState = new XedUserState(webappRoot, principal, submitID, locus);
    }
}
