package io.github.greyp9.arwo.core.xed.state;

import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.xed.clip.XedClipboard;
import io.github.greyp9.arwo.core.xed.session.XedSession;

public class XedUserState {
    private final String submitID;
    private final ViewStates viewStates;
    private final XedSession session;
    private final XedClipboard clipboard;
    private final Locus locus;

    public final String getSubmitID() {
        return submitID;
    }

    public final ViewStates getViewStates() {
        return viewStates;
    }

    public final XedSession getSession() {
        return session;
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
        this.session = session;
        this.clipboard = clipboard;
        this.locus = locus;
    }
}
