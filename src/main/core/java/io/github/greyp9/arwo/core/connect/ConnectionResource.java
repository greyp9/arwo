package io.github.greyp9.arwo.core.connect;

import java.io.IOException;
import java.util.Date;

public interface ConnectionResource {
    String getName();

    Date getDate();

    void close() throws IOException;
}
