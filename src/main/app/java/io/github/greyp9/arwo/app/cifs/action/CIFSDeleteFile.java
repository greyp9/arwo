package io.github.greyp9.arwo.app.cifs.action;

import io.github.greyp9.arwo.app.action.DeferredAction;
import io.github.greyp9.arwo.app.cifs.connection.CIFSConnectionFactory;
import io.github.greyp9.arwo.app.cifs.connection.CIFSConnectionResource;
import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.cifs.data.CIFSDataSource;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.jcifs.fs.connection.CIFSConnection;

import java.io.IOException;

public class CIFSDeleteFile extends DeferredAction {
    private final CIFSRequest request;
    private final AlertActions actions;

    public final AlertActions getActions() {
        return actions;
    }

    public CIFSDeleteFile(final CIFSRequest request) {
        super(Value.join(Http.Token.SLASH, Const.ID, request.getPath()));
        this.request = request;
        this.actions = new AlertActions(getID(), Const.CANCEL, Const.CONFIRM);
    }

    @Override
    public final void doAction(final String option, final Bundle bundle, final Alerts alerts) {
        if (Const.CONFIRM.equals(option)) {
            try {
                final ServletHttpRequest httpRequest = request.getHttpRequest();
                final AppUserState userState = request.getUserState();
                final CIFSConnectionFactory factory = new CIFSConnectionFactory(
                        httpRequest, userState, request.getBundle(), request.getAlerts());
                final CIFSConnectionResource resource = (CIFSConnectionResource)
                        userState.getCIFS().getCache().getResource(request.getServer(), factory);
                final CIFSConnection connection = resource.getConnection();
                final CIFSDataSource source = new CIFSDataSource(request, connection);
                source.delete(request.getPath());
                final String message = bundle.format("WebDAVFileView.file.delete", request.getPath());
                alerts.add(new Alert(Alert.Severity.INFO, message));
            } catch (IOException e) {
                alerts.add(new Alert(Alert.Severity.ERR, e.getMessage(), e.getClass().getName(), null));
            }
        }
    }

    public static class Const {
        private static final String ID = "file.delete";
        private static final String CANCEL = "file.delete.cancel";
        private static final String CONFIRM = "file.delete.confirm";
    }
}
