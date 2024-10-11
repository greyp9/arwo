package io.github.greyp9.arwo.core.vm.env;

import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;

public final class Environment {

    public String systemProperty(final String key) {
        return Value.defaultOnEmpty(System.getProperty(key), "");
    }

    public String environment(final String key) {
        return Value.defaultOnEmpty(System.getenv(key), "");
    }

    public String fileLength(final String path) {
        return Long.toString(new File(path).length());
    }

    public String fileModified(final String path) {
        return Long.toString(new File(path).lastModified());
    }

    public String fileHash(final String path) throws IOException {
        return HexCodec.encode(HashU.sha256(StreamU.read(new File(path))));
    }
}
