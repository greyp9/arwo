package io.github.greyp9.arwo.core.connect;

import java.io.IOException;

public interface ConnectionResource {
    String getName();

    void close() throws IOException;
}
