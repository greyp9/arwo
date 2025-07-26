package io.github.greyp9.arwo.core.envsec.store;

import io.github.greyp9.arwo.core.envsec.EnvironmentSecret;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.util.PropertiesU;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Properties;

public final class SecureStore {
    private final KeyX keyX;
    private final IOException exception;
    private final Properties properties;

    public SecureStore(final File file) {
        KeyX keyXCtor;
        IOException exceptionCtor;
        try {
            keyXCtor = toKeyX(file);
            exceptionCtor = null;
        } catch (IOException e) {
            keyXCtor = null;
            exceptionCtor = e;
        }
        this.keyX = keyXCtor;
        this.exception = exceptionCtor;
        this.properties = new Properties();
    }

    private static KeyX toKeyX(final File file) throws IOException {
        try {
            final byte[] secret = new EnvironmentSecret(file.getPath(), null).recover();
            final SecretKey secretKey = new SecretKeySpec(secret, AES.Const.ALGORITHM);
            return new KeyX(secretKey, KeyX.Const.TRANSFORM_GCM, KeyX.Const.PARAM_SPEC_GCM);
        } catch (GeneralSecurityException e) {
            throw new IOException(e);
        }
    }

    public IOException getException() {
        return exception;
    }

    public void setProperty(final String key, final String value) {
        PropertiesU.setProperty(properties, key, value);
    }

    public void setPropertyProtect(final String key, final String value) throws IOException {
        PropertiesU.setProperty(properties, key, keyX.protect(value));
    }

    public String getProperty(final String key) throws IOException {
        final String valueSecure = properties.getProperty(key);
        return (valueSecure == null) ? null : keyX.unprotect(valueSecure);
    }
}
