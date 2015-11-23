package io.github.greyp9.arwo.core.jce;

import io.github.greyp9.arwo.core.codec.b64.Base64Codec;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;

public final class KeyU {

    private KeyU() {
    }

    // PBE KEY

    public static SecretKey toKeyPBE(
            final char[] secret, final byte[] salt, final int iterations, final int length,
            final String factoryAlgorithm, final String keyAlgorithm) throws GeneralSecurityException {
        final PBEKeySpec keySpec = new PBEKeySpec(secret, salt, iterations, length);
        final SecretKeyFactory factory = SecretKeyFactory.getInstance(factoryAlgorithm);
        final SecretKey secretKey = factory.generateSecret(keySpec);
        return new SecretKeySpec(secretKey.getEncoded(), keyAlgorithm);
    }


    // KEY

    public static String encodeSecretKey(final SecretKey secretKey) throws IOException {
        return Base64Codec.encode(secretKey.getEncoded());
    }

    public static SecretKey decodeSecretKey(final String encoded, final String algorithm) throws IOException {
        return new SecretKeySpec(Base64Codec.decode(encoded), algorithm);
    }

    // CIPHER

    public static Cipher initCipher(
            final Cipher cipher, final int mode, final Key key, final AlgorithmParameterSpec parameterSpec)
            throws GeneralSecurityException {
        if (parameterSpec == null) {
            cipher.init(mode, key);
        } else {
            cipher.init(mode, key, parameterSpec);
        }
        return cipher;
    }

    public static Cipher getCipher(final String transform) throws GeneralSecurityException {
        return Cipher.getInstance(transform);
    }

    public static byte[] getRandomBytes(final int length) {
        final byte[] bytes = new byte[length];
        new SecureRandom().nextBytes(bytes);
        return bytes;
    }
}
