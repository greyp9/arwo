package io.github.greyp9.arwo.core.connect;

import java.io.Closeable;
import java.io.IOException;
import java.util.Date;

public interface ConnectionResource extends Closeable {
    String getName();

    String getID();

    Date getDateOpen();

    Date getDateLast();

    String getTimeout();

    long getCount();

    long getMillis();

    void close() throws IOException;
}
