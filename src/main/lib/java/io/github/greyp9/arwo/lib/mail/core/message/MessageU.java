package io.github.greyp9.arwo.lib.mail.core.message;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;

public final class MessageU {

    private MessageU() {
    }

    public static String toString(final Message message) throws IOException {
        return ((message == null) ? null : toStringNN(message));
    }

    private static String toStringNN(final Message message) throws IOException {
        return ((message instanceof MimeMessage) ? toStringNN((MimeMessage) message) : message.getClass().getName());
    }

    private static String toStringNN(final MimeMessage mimeMessage) throws IOException {
        final StringBuilder buffer = new StringBuilder();
        try {
            final Enumeration<?> headerLines = mimeMessage.getAllHeaderLines();
            while (headerLines.hasMoreElements()) {
                final String header = (String) headerLines.nextElement();
                buffer.append(header);
                buffer.append(Http.Token.CRLF);
            }
            buffer.append(Http.Token.CRLF);
            buffer.append(toStringBody(mimeMessage));
        } catch (MessagingException e) {
            throw new IOException(e);
        }
        return buffer.toString();
    }

    private static String toStringBody(final MimeMessage mimeMessage) throws IOException {
        String body;
        String bodySafer = null;
        try {
            final Object content = mimeMessage.getContent();
            if (content instanceof String) {
                bodySafer = (String) content;
            }
            final InputStream rawInputStream = mimeMessage.getRawInputStream();
            body = UTF8Codec.toString(StreamU.read(rawInputStream));
        } catch (MessagingException e) {
            body = bodySafer;
        }
        return body;
    }
}
