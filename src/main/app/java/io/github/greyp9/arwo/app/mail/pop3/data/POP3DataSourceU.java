package io.github.greyp9.arwo.app.mail.pop3.data;

import javax.mail.Address;

public final class POP3DataSourceU {

    private POP3DataSourceU() {
    }

    public static String toDisplayString(final Address... addresses) {
        final StringBuilder buffer = new StringBuilder();
        if (addresses == null) {
            buffer.append("");
        } else if (addresses.length == 0) {
            buffer.append("");
        } else {
            buffer.append(addresses[0].toString());
        }
        return buffer.toString();
    }

    public static String toDisplay(final Object content) {
        final boolean isString = (content instanceof String);
        return (isString ? ((String) content) : content.getClass().getName());
    }
}
