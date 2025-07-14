package io.github.greyp9.arwo.app.local.sh.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;

import java.io.IOException;

public class SHSelectFavorite {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SHSelectFavorite(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final String doAction(final SubmitToken token) throws IOException {
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final XedNav nav = new XedNav(xed);
        final String xpath = String.format(XPATH_BY_CONTEXT_AND_NAME, token.getObject(), token.getObject2());
        final XedCursor cursor = nav.findX(xpath);
        String context = null;
        if (cursor != null) {
            context = cursor.getValue(cursor.getChildInstance(App.Settings.CONTEXT));
            final String command = cursor.getValue(cursor.getChildInstance(App.Settings.COMMAND));
            userState.getProperties().setProperty(App.Settings.COMMAND, command);
        }
        return PathU.toDir(httpRequest.getBaseURI(), context);
    }

    private static final String XPATH_BY_CONTEXT_AND_NAME =
            "/app:favorites/app:lshFavorites/app:lshFavorite[@context='%s' and @name='%s']";
}
