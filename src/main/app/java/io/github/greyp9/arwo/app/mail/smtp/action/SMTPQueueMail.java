package io.github.greyp9.arwo.app.mail.smtp.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.mail.smtp.connection.SMTPConnectionFactory;
import io.github.greyp9.arwo.app.mail.smtp.connection.SMTPConnectionResource;
import io.github.greyp9.arwo.app.mail.smtp.core.SMTPRequest;
import io.github.greyp9.arwo.app.mail.smtp.data.SMTPDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.action.XedActionMail;
import io.github.greyp9.arwo.core.xed.model.Xed;

import java.io.IOException;

public class SMTPQueueMail {
    private final SMTPRequest request;

    public SMTPQueueMail(final SMTPRequest request) {
        this.request = request;
    }

    public final String doAction(final String locationIn, final NameTypeValues httpArguments) throws IOException {
        final AppUserState userState = request.getUserState();
        final Bundle bundle = request.getBundle();
        final Alerts alerts = request.getAlerts();
        final String server = request.getServer();
        final SMTPConnectionFactory factory = new SMTPConnectionFactory(
                request.getHttpRequest(), request.getUserState(), request.getBundle(), request.getAlerts());
        final ConnectionCache cache = userState.getMail().getCacheSMTP();
        final SMTPConnectionResource resource = (SMTPConnectionResource) cache.getResource(server, factory);
        if (resource == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SFTPHandlerPostMultipart.no.connect", server)));
        } else {
            final Xed xed = new XedActionMail(null).update(httpArguments);
            new SMTPDataSource(request, resource.getConnection()).sendMessage(xed);
        }
        return locationIn;
    }
}
