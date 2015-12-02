package io.github.greyp9.arwo.core.lang;

import java.util.ArrayList;
import java.util.Collection;

public final class ExceptionU {

    private ExceptionU() {
    }

    public static Collection<String> getMessages(final Throwable t) {
        Throwable throwable = t;
        final ArrayList<String> messages = new ArrayList<String>();
        while (throwable != null) {
            final String message = throwable.getMessage();
            if ((message != null) && (!messages.contains(message))) {
                messages.add(message);
            }
            throwable = throwable.getCause();
        }
        return messages;
    }
}
