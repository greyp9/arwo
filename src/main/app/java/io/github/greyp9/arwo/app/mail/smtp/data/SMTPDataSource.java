package io.github.greyp9.arwo.app.mail.smtp.data;

import io.github.greyp9.arwo.app.mail.smtp.core.SMTPRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.result.io.ResultsPersister;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.lib.mail.core.message.MessageU;
import io.github.greyp9.arwo.lib.mail.smtp.connection.SMTPConnection;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.io.IOException;
import java.util.Date;

public class SMTPDataSource {
    private final SMTPRequest request;
    private final SMTPConnection connection;
    private final Bundle bundle;
    private final Alerts alerts;

    public SMTPDataSource(final SMTPRequest request, final SMTPConnection connection) {
        this.request = request;
        this.connection = connection;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final void sendMessage(final Xed message) throws IOException {
        final XPather xpather = new XPather(message.getDocument(), message.getXsdTypes().getContext());
        final String to = xpather.getText("/action:mail/action:to");
        final String cc = xpather.getText("/action:mail/action:cc");
        final String bcc = xpather.getText("/action:mail/action:bcc");
        final String subject = xpather.getText("/action:mail/action:subject");
        final String body = xpather.getText("/action:mail/action:body");
        try {
            final Date date = new Date();
            final Session session = connection.getSession();
            final Transport transport = connection.getTransport(session);
            final MimeMessage mimeMessage = new MimeMessage(session);
            mimeMessage.setFrom(new InternetAddress(connection.getFrom()));
            addRecipient(to, Message.RecipientType.TO, mimeMessage);
            addRecipient(cc, Message.RecipientType.CC, mimeMessage);
            addRecipient(bcc, Message.RecipientType.BCC, mimeMessage);
            mimeMessage.setSentDate(request.getHttpRequest().getDate());
            mimeMessage.setSubject(subject, UTF8Codec.Const.UTF8);
            mimeMessage.setText(body, UTF8Codec.Const.UTF8);
            mimeMessage.saveChanges();
            transport.sendMessage(mimeMessage, mimeMessage.getAllRecipients());
            transport.close();
            connection.update(date);
            alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                    "SMTPDataSource.message.sent", request.getServer(), subject)));
            // optionally persist fetched results
            final byte[] bytes = UTF8Codec.toBytes(MessageU.toString(mimeMessage));
            new ResultsPersister(request.getUserState().getResultsContext(request.getHttpRequest())).write(bytes);
        } catch (MessagingException e) {
            new ExceptionModel(alerts).service(new IOException(e), Alert.Severity.ERR);
        }
    }

    private void addRecipient(final String address, final Message.RecipientType type, final MimeMessage mimeMessage)
            throws MessagingException {
        if (!Value.isEmpty(address)) {
            mimeMessage.setRecipient(type, new InternetAddress(address));
        }
    }
}
