package io.github.greyp9.arwo.app.ssh.sftp.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
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

public class SFTPAddFavorite {
    private final SFTPRequest request;

    public SFTPAddFavorite(final SFTPRequest request) {
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
            final XedCursor cursorFavorites = nav.findX("/app:favorites/app:sftp-favorites");
            final TypeInstance typeInstance = cursorFavorites.getChildInstance("sftp-favorite");
            final NameTypeValues ntv = NameTypeValuesU.create(
                    "sftp-favorite.sftpFavoriteType.server", server,
                    "sftp-favorite.sftpFavoriteType.resource", resource,
                    "sftp-favorite.sftpFavoriteType.comment", comment);
            final ValueInstance valueInstance = ValueInstance.create(typeInstance, ntv);
            new OpCreate(null, session.getXed()).apply(cursorFavorites.getElement(), valueInstance);
            final XedRequest xedRequest = new XedRequest(request.getHttpRequest(), session, documentState);
            new SessionSave(xedRequest, request.getBundle(), request.getAlerts()).save();
        }
        return null;
    }
}
