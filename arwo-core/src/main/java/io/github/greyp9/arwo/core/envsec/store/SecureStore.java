package io.github.greyp9.arwo.core.envsec.store;

import io.github.greyp9.arwo.core.envsec.EnvironmentSecret;
import io.github.greyp9.arwo.core.jce.AES;
import io.github.greyp9.arwo.core.jce.KeyX;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Properties;
import java.util.logging.Logger;

public final class SecureStore extends Properties {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final KeyX keyX;
    private final IOException exception;

    public SecureStore(final File file, final Properties defaults) {
        super(defaults);
        KeyX keyXCtor;
        IOException exceptionCtor;
        try {
            keyXCtor = (file == null) ? toKeyX((File) null) : toKeyX(file);
            exceptionCtor = null;
        } catch (IOException e) {
            keyXCtor = null;
            exceptionCtor = e;
        }
        this.keyX = keyXCtor;
        this.exception = exceptionCtor;
    }

    public SecureStore(final File file) {
        this(file, null);
    }

    public SecureStore() {
        this(null);
    }

    private static KeyX toKeyX(final File file) throws IOException {
        try {
            final byte[] recover = (file == null)
                    ? AES.generate().getEncoded()
                    : new EnvironmentSecret(file.getPath(), null).recover();
            return toKeyX(recover);
        } catch (GeneralSecurityException e) {
            throw new IOException(e);
        }
    }

    private static KeyX toKeyX(final byte[] secret) {
        final SecretKey secretKey = new SecretKeySpec(secret, AES.Const.ALGORITHM);
        return new KeyX(secretKey, KeyX.Const.TRANSFORM_GCM, KeyX.Const.PARAM_SPEC_GCM);
    }

    public IOException getException() {
        return exception;
    }

    @Override
    public synchronized Object setProperty(final String key, final String value) {
        try {
            return super.setProperty(key, keyX.protect(value));
        } catch (IOException e) {
            logger.warning(e.getMessage());
            return null;
        }
    }

    @Override
    public String getProperty(final String key) {
        try {
            final String value = super.getProperty(key);
            return (value == null) ? null : keyX.unprotect(value);
        } catch (IOException e) {
            logger.warning(e.getMessage());
            return null;
        }
    }

    public String getPropertyUnsafe(final String key) throws IOException {
        final String value = super.getProperty(key);
        return (value == null) ? null : keyX.unprotect(value);
    }
}
