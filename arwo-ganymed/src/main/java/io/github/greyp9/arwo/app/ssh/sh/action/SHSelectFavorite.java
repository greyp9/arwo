package io.github.greyp9.arwo.app.ssh.sh.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.sh.core.SHRequest;
import io.github.greyp9.arwo.core.app.App;
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
        final XedSession session = documentState.getSession(App.Servlet.FAVORITES);
        final XedNav nav = new XedNav(session.getXed());
        final XedCursor cursor = nav.find(uri);
        final String server = cursor.getValue(cursor.getChildInstance(App.Settings.SERVER));
        final String command = cursor.getValue(cursor.getChildInstance(App.Settings.COMMAND));
        userState.getSSH().getProperties().setProperty(App.Settings.COMMAND, command);
        return String.format("%s/%s", request.getHttpRequest().getBaseURI(), server);
    }
}
