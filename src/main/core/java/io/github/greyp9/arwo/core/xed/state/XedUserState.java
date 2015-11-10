package io.github.greyp9.arwo.core.xed.state;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppFolder;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.locus.LocusFactory;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.session.XedEntries;
import io.github.greyp9.arwo.core.xed.session.XedEntry;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.XedSessions;

import java.io.File;
import java.io.IOException;

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

    public XedUserState(final String contextPath, final String submitID, final ViewStates viewStates,
                        final XedClipboard clipboard, final Locus locus) throws IOException {
        this.submitID = submitID;
        this.viewStates = viewStates;
        final File webappRoot = AppFolder.getWebappRoot(contextPath);
        final File realmFile = new File(webappRoot, "root/realm.xml");
        final String xmlPath = realmFile.getCanonicalPath();
        final String xsdPathRealm = ResourceU.resolve(App.Realm.XSD).toExternalForm();
        final String xsdPathActions = ResourceU.resolve(App.Actions.XSD).toExternalForm();
        final XedEntry entryRealm = new XedEntry("/users", App.Realm.QNAME, xmlPath, xsdPathRealm, null);
        final XedEntry entryFilter = new XedEntry("/filter", App.Actions.QNAME_FILTER, null, xsdPathActions, null);
        final XedEntry entryLocale = new XedEntry("/locale", App.Actions.QNAME_LOCALE, null, xsdPathActions, null);
        final XedEntries entries = new XedEntries(entryRealm, entryFilter, entryLocale);
        this.sessions = new XedSessions(entries);
        this.clipboard = clipboard;
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
