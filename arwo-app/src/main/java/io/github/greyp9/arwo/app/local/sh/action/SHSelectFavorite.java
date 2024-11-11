package io.github.greyp9.arwo.app.local.sh.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;

import java.io.IOException;

public class SHSelectFavorite {
    private final SHRequest request;

    public SHSelectFavorite(final SHRequest request) {
        this.request = request;
    }

    public final String doAction(final SubmitToken token) throws IOException {
        final AppUserState userState = request.getUserState();
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final XedNav nav = new XedNav(xed);
        final String xpath = String.format(XPATH_BY_CONTEXT_AND_NAME, token.getObject(), token.getObject2());
        final XedCursor cursor = nav.findX(xpath);
        String context = null;
        if (cursor != null) {
            context = cursor.getValue(cursor.getChildInstance(App.Settings.CONTEXT));
            final String command = cursor.getValue(cursor.getChildInstance(App.Settings.COMMAND));
            userState.getLocal().getProperties().setProperty(App.Settings.COMMAND, command);
        }
        return PathU.toDir(request.getHttpRequest().getBaseURI(), context);
    }

    private static final String XPATH_BY_CONTEXT_AND_NAME =
            "/app:favorites/app:lshFavorites/app:lshFavorite[@context='%s' and @name='%s']";
}
