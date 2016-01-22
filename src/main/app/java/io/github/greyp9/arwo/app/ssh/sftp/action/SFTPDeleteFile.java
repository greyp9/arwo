package io.github.greyp9.arwo.app.ssh.sftp.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionFactory;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.core.actiond.DeferredAction;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.io.IOException;

public class SFTPDeleteFile extends DeferredAction {
    private final SFTPRequest request;
    private final AlertActions actions;

    public final AlertActions getActions() {
        return actions;
    }

    public SFTPDeleteFile(final SFTPRequest request) {
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
                final SSHConnectionFactory factory = new SSHConnectionFactory(
                        httpRequest, userState, request.getBundle(), request.getAlerts());
                final SSHConnectionResource resource = (SSHConnectionResource)
                        userState.getWebDAV().getCache().getResource(request.getServer(), factory);
                final SSHConnection connection = resource.getConnection();
                final SFTPDataSource source = new SFTPDataSource(request, connection);
                source.delete(request.getPath());
                final String message = bundle.format("WebDAVFileView.file.delete", request.getPath());
                alerts.add(new Alert(Alert.Severity.INFO, message));
            } catch (IOException e) {
                alerts.add(new Alert(Alert.Severity.ERR, e.getMessage(), e.getClass().getName()));
            }
        }
    }

    public static class Const {
        private static final String ID = "file.delete";
        private static final String CANCEL = "file.delete.cancel";
        private static final String CONFIRM = "file.delete.confirm";
    }
}
