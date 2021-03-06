package io.github.greyp9.arwo.core.jce;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.Key;

public class KeyX {
    private final KeyCodec keyCodec;

    public KeyX(final Key key, final String transform) {
        this.keyCodec = new KeyCodec(key, transform);
    }

    public final String protect(final String value) throws IOException {
        try {
            final byte[] bytesClear = UTF8Codec.toBytes(value);
            final byte[] bytesCrypt = keyCodec.encode(bytesClear);
            return Base64Codec.encode(bytesCrypt);
        } catch (GeneralSecurityException e) {
            throw new IOException(e);
        }
    }

    public final String unprotect(final String value) throws IOException {
        try {
            final byte[] bytesCrypt = Base64Codec.decode(value);
            final byte[] bytesClear = keyCodec.decode(bytesCrypt);
            return UTF8Codec.toString(bytesClear);
        } catch (GeneralSecurityException e) {
            throw new IOException(e);
        }
    }
}
