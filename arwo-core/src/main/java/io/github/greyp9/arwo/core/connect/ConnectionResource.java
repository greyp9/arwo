package io.github.greyp9.arwo.core.connect;

import java.io.IOException;
import java.util.Date;

public interface ConnectionResource {
    String getName();

    String getID();

    Date getDateOpen();

    Date getDateLast();

    long getCount();

    long getMillis();

    void close() throws IOException;
}
