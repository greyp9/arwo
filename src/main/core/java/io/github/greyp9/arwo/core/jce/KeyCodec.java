package io.github.greyp9.arwo.core.jce;

import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.value.Value;

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
        return (Value.isEmpty(bytes) ? bytes : (isSymmetric() ? encodeSymmetric(bytes) : encodeAsymmetric(bytes)));
    }

    public final byte[] encode(final byte[] bytes, final IvParameterSpec ivParameterSpec)
            throws GeneralSecurityException {
        return (Value.isEmpty(bytes) ? bytes : (isSymmetric()
                ? encodeSymmetric(bytes, ivParameterSpec) : encodeAsymmetric(bytes)));
    }

    private boolean isSymmetric() {
        return (key instanceof SecretKey);
    }

    private byte[] encodeSymmetric(final byte[] bytes) throws GeneralSecurityException {
        final byte[] ivBytes = KeyU.getRandomBytes(KeyU.Const.AES_SIZE_IV);
        final IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);
        return encodeSymmetric(bytes, ivParameterSpec);
    }

    private byte[] encodeSymmetric(final byte[] bytes, final IvParameterSpec ivParameterSpec)
            throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.ENCRYPT_MODE, key, ivParameterSpec);
        return ByteU.join(ivParameterSpec.getIV(), cipherInit.doFinal(bytes, 0, bytes.length));
    }

    private byte[] encodeAsymmetric(final byte[] bytes) throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.ENCRYPT_MODE, key, null);
        return cipherInit.doFinal(bytes, 0, bytes.length);
    }

    public final byte[] decode(final byte[] bytes) throws GeneralSecurityException {
        return (Value.isEmpty(bytes) ? bytes : (isSymmetric() ? decodeSymmetric(bytes) : decodeAsymmetric(bytes)));
    }

    public final byte[] decode(final byte[] bytes, final IvParameterSpec ivParameterSpec)
            throws GeneralSecurityException {
        return (Value.isEmpty(bytes) ? bytes : (isSymmetric()
                ? decodeSymmetric(bytes, ivParameterSpec) : decodeAsymmetric(bytes)));
    }

    private byte[] decodeSymmetric(final byte[] bytes) throws GeneralSecurityException {
        final IvParameterSpec ivParameterSpec = new IvParameterSpec(bytes, 0, KeyU.Const.AES_SIZE_IV);
        return decodeSymmetric(bytes, ivParameterSpec);
    }

    private byte[] decodeSymmetric(final byte[] bytes, final IvParameterSpec ivParameterSpec)
            throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.DECRYPT_MODE, key, ivParameterSpec);
        return cipherInit.doFinal(bytes, KeyU.Const.AES_SIZE_IV, (bytes.length - KeyU.Const.AES_SIZE_IV));
    }

    private byte[] decodeAsymmetric(final byte[] bytes) throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.DECRYPT_MODE, key, null);
        return cipherInit.doFinal(bytes, 0, bytes.length);
    }
}
