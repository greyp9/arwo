package io.github.greyp9.arwo.core.lang;

import java.io.IOException;
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

    public static void assertEquals(final long expected, final long actual) throws IOException {
        if (expected != actual) {
            throw new IOException(String.format("failure, assertEquals(%d,%d)", expected, actual));
        }
    }

    public static void assertNotNull(final Object actual) throws IOException {
        if (actual == null) {
            throw new IOException("failure, assertNotNull()");
        }
    }
}
