package io.github.greyp9.arwo.app.local.fs.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.html.Html;
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

public class LFSAddFavorite {
    private final LFSRequest request;

    public LFSAddFavorite(final LFSRequest request) {
        this.request = request;
    }

    public final String doAction() throws IOException {
        final String folder = request.getFolder();
        final String resource = request.getPath();
        final String comment = XsdDateU.toXSDZ(request.getHttpRequest().getDate());
        final boolean isData = Value.isData(resource);
        if (isData) {
            final AppUserState userState = request.getUserState();
            final XedUserState documentState = userState.getDocumentState();
            final XedSession session = documentState.getSession(App.Servlet.FAVORITES);
            final XedNav nav = new XedNav(session.getXed());
            final XedCursor cursorFavorites = nav.findX("/app:favorites/app:lfsFavorites");  // i18n xpath
            final TypeInstance typeInstance = cursorFavorites.getChildInstance("lfsFavorite");  // i18n xpath
            final NameTypeValues ntv = NameTypeValuesU.create(
                    "lfsFavorite.lfsFavoriteType.folder", folder,
                    "lfsFavorite.lfsFavoriteType.resource", resource,
                    "lfsFavorite.lfsFavoriteType.comment", comment,
                    "lfsFavorite.lfsFavoriteType.enabled", Html.ON);
            final ValueInstance valueInstance = ValueInstance.create(typeInstance, ntv);
            new OpCreate(null, session.getXed()).apply(cursorFavorites.getElement(), valueInstance);
            final XedRequest xedRequest = new XedRequest(request.getHttpRequest(), session, documentState);
            new SessionSave(xedRequest, request.getBundle(), request.getAlerts()).save();
        }
        return null;
    }
}
