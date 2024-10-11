package io.github.greyp9.arwo.core.connect;

import java.io.IOException;

public interface ConnectionFactory {
    ConnectionResource create(String name) throws IOException;
}
