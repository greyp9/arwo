package io.github.greyp9.arwo.core.xed.state;

import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.locus.LocusFactory;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessions;
import io.github.greyp9.arwo.core.xed.session.XedSessionsFactory;

import java.io.File;
import java.io.IOException;
import java.security.Principal;

public class XedUserState {
    private final String submitID;
    private final ViewStates viewStates;
    private final XedSessions sessions;
    private final XedClipboard clipboard;

    private Locus locus;

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final XedSession getSession(final String contextPath) throws IOException {
        return sessions.getSession(contextPath, locus.getLocale());
    }

    public final XedClipboard getClipboard() {
        return clipboard;
    }

    public final Locus getLocus() {
        return locus;
    }

    public XedUserState(final File webappRoot, final Principal principal, final String submitID, final Locus locus)
            throws IOException {
        this.submitID = submitID;
        this.viewStates = new ViewStates();
        this.sessions = new XedSessionsFactory(webappRoot).getSessions(principal, locus);
        this.clipboard = new XedClipboard();
        this.locus = locus;
    }

    public final void apply(final SubmitToken token, final NameTypeValues nameTypeValues) throws IOException {
        final String object = token.getObject();
        if ("locale".equals(object)) {
            applyLocale(nameTypeValues);
        }
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void applyLocale(final NameTypeValues nameTypeValues) throws IOException {
        // apply to user state
        final Xed actionLocale = new XedActionLocale(null).update(nameTypeValues);
        final String localeID = actionLocale.getXPather().getText("/action:locale");
        locus = new LocusFactory().create(localeID, locus.getDateX());
        // apply to each xed session
        sessions.applyLocale(locus.getLocale());
    }
}
