package io.github.greyp9.arwo.core.http.form;

import io.github.greyp9.arwo.core.http.Http;

import java.util.Properties;

public class MimeHeader {
    private final String line;

/*
    public String getLine() {
        return line;
    }
*/

    public MimeHeader(final String line) {
        this.line = line;
    }

    public final void addTo(final Properties properties) {
        final int colon = line.indexOf(Http.Token.COLON);
        if (colon >= 0) {
            final String name = line.substring(0, colon);
            final String value = line.substring(colon + Http.Token.COLON.length());
            decodeHeader(properties, name, value.trim());
        }
    }

    private void decodeHeader(final Properties headers, final String name, final String value) {
        final String[] attributes = value.split(Http.Token.SEMICOLON);
        for (final String attribute : attributes) {
            final int equals = attribute.indexOf(Http.Token.EQUALS);
            if (equals < 0) {
                decodeHeaderAttribute(headers, name, attribute);
            } else {
                final String attributeName = attribute.substring(0, equals).trim();
                final String attributeValue = attribute.substring(equals + Http.Token.EQUALS.length()).trim();
                decodeHeaderAttribute(headers, name + "." + attributeName, attributeValue);
            }
        }
    }

    private void decodeHeaderAttribute(
            final Properties properties, final String attributeName, final String attributeValue) {
        final String quote = Http.Token.QUOTE;
        if ((attributeValue.startsWith(quote)) && (attributeValue.endsWith(quote))) {
            final int begin = quote.length();
            final int end = attributeValue.length() - quote.length();
            properties.setProperty(attributeName, attributeValue.substring(begin, end));
        } else {
            properties.setProperty(attributeName, attributeValue);
        }
    }
}
