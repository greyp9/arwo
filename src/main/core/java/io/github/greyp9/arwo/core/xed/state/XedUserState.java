package io.github.greyp9.arwo.core.xed.state;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.locus.LocusFactory;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.session.XedSession;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

public class XedUserState {
    private final String submitID;
    private final ViewStates viewStates;
    private final Map<String, XedSession> sessions;
    private final XedClipboard clipboard;

    private Locus locus;

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final XedSession getSession() {
        return sessions.get(App.Realm.QNAME.toString());
    }

    public final XedClipboard getClipboard() {
        return clipboard;
    }

    public final Locus getLocus() {
        return locus;
    }

    public XedUserState(final String submitID, final ViewStates viewStates,
                        final XedSession session, final XedClipboard clipboard, final Locus locus) {
        this.submitID = submitID;
        this.viewStates = viewStates;
        this.sessions = new TreeMap<String, XedSession>();
        this.sessions.put(App.Realm.QNAME.toString(), session);
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
        for (final Map.Entry<String, XedSession> entry : sessions.entrySet()) {
            final XedSession session = entry.getValue();
            final Xed xed = session.getXed();
            final Xed xedUpdate = new Xed(xed.getDocument(), xed.getXsdTypes(), locus.getLocale());
            final XedSession sessionUpdate = new XedSession(
                    session.getEntry(), xedUpdate, session.getFile(), session.getDateLoad());
            this.sessions.put(entry.getKey(), sessionUpdate);
        }
    }
}
