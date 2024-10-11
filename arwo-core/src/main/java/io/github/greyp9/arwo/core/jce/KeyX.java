package io.github.greyp9.arwo.core.jce;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;

import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.IvParameterSpec;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.SecureRandom;
import java.util.Random;

public class KeyX {
    private final Random random;
    private final KeyCodec keyCodec;

    public KeyX(final Key key, final String transform, final String parameterSpec) {
        this.random = new SecureRandom();
        this.keyCodec = new KeyCodec(key, transform, parameterSpec, random);
    }

    public final boolean hasKey() {
        return keyCodec.hasKey();
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

    public static class Const {
        public static final String TRANSFORM_CTR = "AES/CTR/NoPadding";
        public static final String TRANSFORM_GCM = "AES/GCM/NoPadding";

        public static final String PARAM_SPEC_IV = IvParameterSpec.class.getSimpleName();
        public static final String PARAM_SPEC_GCM = GCMParameterSpec.class.getSimpleName();
    }
}
