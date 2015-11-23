package io.github.greyp9.arwo.core.jce;

import io.github.greyp9.arwo.core.io.ByteU;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import java.security.GeneralSecurityException;
import java.security.Key;

public class KeyCodec {
    private final Key key;
    private final String transform;

    public KeyCodec(final Key key, final String transform) {
        this.key = key;
        this.transform = transform;
    }

    public final byte[] encode(final byte[] bytes) throws GeneralSecurityException {
        return (isSymmetric() ? encodeSymmetric(bytes) : encodeAsymmetric(bytes));
    }

    private boolean isSymmetric() {
        return (key instanceof SecretKey);
    }

    private byte[] encodeSymmetric(final byte[] bytes) throws GeneralSecurityException {
        final int length = key.getEncoded().length;
        final byte[] ivBytes = KeyU.getRandomBytes(length);
        final IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.ENCRYPT_MODE, key, ivParameterSpec);
        return ByteU.join(ivBytes, cipherInit.doFinal(bytes, 0, bytes.length));
    }

    private byte[] encodeAsymmetric(final byte[] bytes) throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.ENCRYPT_MODE, key, null);
        return cipherInit.doFinal(bytes, 0, bytes.length);
    }

    public final byte[] decode(final byte[] bytes) throws GeneralSecurityException {
        return ((bytes.length == 0) ? bytes : (isSymmetric() ? decodeSymmetric(bytes) : decodeAsymmetric(bytes)));
    }

    private byte[] decodeSymmetric(final byte[] bytes) throws GeneralSecurityException {
        final int length = key.getEncoded().length;
        final IvParameterSpec ivParameterSpec = new IvParameterSpec(bytes, 0, length);
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.DECRYPT_MODE, key, ivParameterSpec);
        return cipherInit.doFinal(bytes, length, (bytes.length - length));
    }

    private byte[] decodeAsymmetric(final byte[] bytes) throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.DECRYPT_MODE, key, null);
        return cipherInit.doFinal(bytes, 0, bytes.length);
    }
}
