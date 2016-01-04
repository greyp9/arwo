package io.github.greyp9.arwo.app.webdav.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpCreate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xed.session.action.SessionSave;
import io.github.greyp9.arwo.core.xed.state.XedUserState;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import java.io.IOException;

public class WebDAVAddFavorite {
    private final WebDAVRequest request;

    public WebDAVAddFavorite(final WebDAVRequest request) {
        this.request = request;
    }

    public final String doAction() throws IOException {
        final String server = request.getServer();
        final String resource = request.getPath();
        final String comment = XsdDateU.toXSDZ(request.getHttpRequest().getDate());
        final boolean isData = (Value.isData(server) && Value.isData(resource));
        if (isData) {
            final AppUserState userState = request.getUserState();
            final XedUserState documentState = userState.getDocumentState();
            final XedSession session = documentState.getSession("/fav");
            final XedNav nav = new XedNav(session.getXed());
            final XedCursor cursorFavorites = nav.findX("/app:favorites/app:webdav-favorites");
            final TypeInstance typeInstance = cursorFavorites.getChildInstance("webdav-favorite");
            final NameTypeValues ntv = NameTypeValuesU.create(
                    "webdav-favorite.webdavFavoriteType.server", server,
                    "webdav-favorite.webdavFavoriteType.resource", resource,
                    "webdav-favorite.webdavFavoriteType.comment", comment);
            final ValueInstance valueInstance = ValueInstance.create(typeInstance, ntv);
            new OpCreate(null, session.getXed()).apply(cursorFavorites.getElement(), valueInstance);
            final XedRequest xedRequest = new XedRequest(request.getHttpRequest(), session, documentState);
            new SessionSave(xedRequest, request.getBundle(), request.getAlerts()).save();
        }
        return null;
    }
}
