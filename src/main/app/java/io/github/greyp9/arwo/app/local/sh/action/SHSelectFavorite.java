package io.github.greyp9.arwo.app.local.sh.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.state.XedUserState;

import java.io.IOException;

public class SHSelectFavorite {
    private final SHRequest request;

    public SHSelectFavorite(final SHRequest request) {
        this.request = request;
    }

    public final String doAction(final SubmitToken token) throws IOException {
        final String uri = token.getObject();
        final AppUserState userState = request.getUserState();
        final XedUserState documentState = userState.getDocumentState();
        final XedSession session = documentState.getSession("/fav");
        final XedNav nav = new XedNav(session.getXed());
        final XedCursor cursor = nav.find(uri);
        final String command = cursor.getValue(cursor.getChildInstance("command"));
        userState.getLocal().getProperties().setProperty("command", command);
        return request.getHttpRequest().getBaseURI();
    }
}
