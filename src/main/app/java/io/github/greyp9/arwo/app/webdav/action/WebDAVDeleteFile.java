package io.github.greyp9.arwo.app.webdav.action;

import io.github.greyp9.arwo.app.action.DeferredAction;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.webdav.connection.WebDAVConnectionFactory;
import io.github.greyp9.arwo.app.webdav.connection.WebDAVConnectionResource;
import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.app.webdav.fs.data.WebDAVDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.WebDAVConnection;

import java.io.IOException;

public class WebDAVDeleteFile extends DeferredAction {
    private final WebDAVRequest request;
    private final AlertActions actions;

    public final AlertActions getActions() {
        return actions;
    }

    public WebDAVDeleteFile(final WebDAVRequest request) {
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
                final WebDAVConnectionFactory factory = new WebDAVConnectionFactory(
                        httpRequest, userState, request.getBundle(), request.getAlerts());
                final WebDAVConnectionResource resource = (WebDAVConnectionResource)
                        userState.getWebDAV().getCache().getResource(request.getServer(), factory);
                final WebDAVConnection webDAVConnection = resource.getConnection();
                final WebDAVDataSource source = new WebDAVDataSource(request, webDAVConnection);
                source.delete(request.getPathURL());
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
