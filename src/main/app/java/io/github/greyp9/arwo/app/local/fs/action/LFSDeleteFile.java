package io.github.greyp9.arwo.app.local.fs.action;

import io.github.greyp9.arwo.app.action.DeferredAction;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.action.AlertActions;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class LFSDeleteFile extends DeferredAction {
    private final LFSRequest request;
    private final AlertActions actions;

    public final AlertActions getActions() {
        return actions;
    }

    public LFSDeleteFile(final LFSRequest request) {
        super(Value.join(Http.Token.SLASH, Const.ID, request.getPath()));
        this.request = request;
        this.actions = new AlertActions(getID(), Const.CANCEL, Const.CONFIRM);
    }

    @Override
    public final void doAction(final String option, final Bundle bundle, final Alerts alerts) {
        if (Const.CONFIRM.equals(option)) {
            try {
                final LFSDataSource source = new LFSDataSource(request, request.getUserState().getUserRoot());
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
