package io.github.greyp9.arwo.app.interop.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.interop.core.SHRequest;
import io.github.greyp9.arwo.core.app.App;
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
import java.util.Properties;

public class SHAddFavorite {
    private final SHRequest request;

    public SHAddFavorite(final SHRequest request) {
        this.request = request;
    }

    public final String doAction() throws IOException {
        final String server = request.getServer();
        final Properties properties = request.getUserState().getInterop().getProperties();
        final String command = properties.getProperty(App.Settings.COMMAND, "");
        final String comment = XsdDateU.toXSDZ(request.getHttpRequest().getDate());
        final boolean isData = (Value.isData(server) && Value.isData(command));
        if (isData) {
            final AppUserState userState = request.getUserState();
            final XedUserState documentState = userState.getDocumentState();
            final XedSession session = documentState.getSession(App.Servlet.FAVORITES);
            final XedNav nav = new XedNav(session.getXed());
            final XedCursor cursorFavorites = nav.findX("/app:favorites/app:wshFavorites");  // i18n xpath
            final TypeInstance typeInstance = cursorFavorites.getChildInstance("wshFavorite");  // i18n xpath
            final NameTypeValues ntv = NameTypeValuesU.create(
                    "wshFavorite.wshFavoriteType.server", server,
                    "wshFavorite.wshFavoriteType.command", command,
                    "wshFavorite.wshFavoriteType.comment", comment);
            final ValueInstance valueInstance = ValueInstance.create(typeInstance, ntv);
            new OpCreate(null, session.getXed()).apply(cursorFavorites.getElement(), valueInstance);
            final XedRequest xedRequest = new XedRequest(request.getHttpRequest(), session, documentState);
            new SessionSave(xedRequest, request.getBundle(), request.getAlerts()).save();
        }
        return null;
    }
}
