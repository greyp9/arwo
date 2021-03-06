package io.github.greyp9.arwo.app.cifs.action;

import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import java.io.IOException;

public class CIFSSelectFavorite {
    private final CIFSRequest request;

    public CIFSSelectFavorite(final CIFSRequest request) {
        this.request = request;
    }

    public final String doAction(final SubmitToken token) throws IOException {
        final String uri = token.getObject();
        final AppUserState userState = request.getUserState();
        final XedUserState documentState = userState.getDocumentState();
        final XedSession session = documentState.getSession(App.Servlet.FAVORITES);
        final XedNav nav = new XedNav(session.getXed());
        final XedCursor cursor = nav.find(uri);
        final String server = cursor.getValue(cursor.getChildInstance(App.Settings.SERVER));
        final String resource = cursor.getValue(cursor.getChildInstance(App.Settings.RESOURCE));
        return String.format("%s/%s%s", request.getBaseURIMode(), server, resource);
    }
}
