package io.github.greyp9.arwo.core.jce;

import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.value.Value;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.IvParameterSpec;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Random;

public class KeyCodec {
    private final Key key;
    private final String transform;
    private final String parameterSpec;
    private final Random random;

    public KeyCodec(final Key key, final String transform, final String parameterSpec, final Random random) {
        this.key = key;
        this.transform = transform;
        this.parameterSpec = parameterSpec;
        this.random = random;
    }

    public final byte[] encode(final byte[] bytes) throws GeneralSecurityException {
        return (Value.isEmpty(bytes) ? bytes : (isSymmetric() ? encodeSymmetric(bytes) : encodeAsymmetric(bytes)));
    }

    private boolean isSymmetric() {
        return (key instanceof SecretKey);
    }

    private byte[] encodeSymmetric(final byte[] bytes) throws GeneralSecurityException {
        final byte[] encoded;
        if (IvParameterSpec.class.getSimpleName().equals(parameterSpec)) {
            final byte[] ivBytes = KeyU.getRandomBytes(AES.Const.IV_BYTES_CTR, random);
            final IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);
            encoded = encodeSymmetric(bytes, ivParameterSpec);
        } else if (GCMParameterSpec.class.getSimpleName().equals(parameterSpec)) {
            final byte[] iv = KeyU.getRandomBytes(AES.Const.IV_BYTES_GCM, random);
            final GCMParameterSpec gcmParameterSpec = new GCMParameterSpec(AES.Const.TAG_BYTES_GCM * Byte.SIZE, iv);
            encoded = encodeSymmetric(bytes, gcmParameterSpec);
        } else if (null == parameterSpec) {
            encoded = encodeSymmetricNoSpec(bytes);
        } else {
            throw new GeneralSecurityException(new IllegalArgumentException(AlgorithmParameterSpec.class.getName()));
        }
        return encoded;
    }

    private byte[] encodeSymmetricNoSpec(final byte[] bytes) throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.ENCRYPT_MODE, key, null);
        return cipherInit.doFinal(bytes, 0, bytes.length);
    }

    private byte[] encodeSymmetric(final byte[] bytes, final IvParameterSpec ivParameterSpec)
            throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.ENCRYPT_MODE, key, ivParameterSpec);
        return ByteU.join(ivParameterSpec.getIV(), cipherInit.doFinal(bytes, 0, bytes.length));
    }

    private byte[] encodeSymmetric(final byte[] bytes, final GCMParameterSpec gcmParameterSpec)
            throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.ENCRYPT_MODE, key, gcmParameterSpec);
        return ByteU.join(gcmParameterSpec.getIV(), cipherInit.doFinal(bytes, 0, bytes.length));
    }

    private byte[] encodeAsymmetric(final byte[] bytes) throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.ENCRYPT_MODE, key, null);
        return cipherInit.doFinal(bytes, 0, bytes.length);
    }

    public final byte[] decode(final byte[] bytes) throws GeneralSecurityException {
        return (Value.isEmpty(bytes) ? bytes : (isSymmetric() ? decodeSymmetric(bytes) : decodeAsymmetric(bytes)));
    }

    private byte[] decodeSymmetric(final byte[] bytes) throws GeneralSecurityException {
        final byte[] decoded;
        if (IvParameterSpec.class.getSimpleName().equals(parameterSpec)) {
            final IvParameterSpec ivParameterSpec = new IvParameterSpec(bytes, 0, AES.Const.IV_BYTES_CTR);
            decoded = decodeSymmetric(bytes, ivParameterSpec);
        } else if (GCMParameterSpec.class.getSimpleName().equals(parameterSpec)) {
            final GCMParameterSpec gcmParameterSpec = new GCMParameterSpec(AES.Const.TAG_BYTES_GCM * Byte.SIZE,
                    bytes, 0, AES.Const.IV_BYTES_GCM);
            decoded = decodeSymmetric(bytes, gcmParameterSpec);
        } else if (null == parameterSpec) {
            decoded = decodeSymmetricNoSpec(bytes);
        } else {
            throw new GeneralSecurityException(new IllegalArgumentException(AlgorithmParameterSpec.class.getName()));
        }
        return decoded;
    }

    private byte[] decodeSymmetricNoSpec(final byte[] bytes) throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.DECRYPT_MODE, key, null);
        return cipherInit.doFinal(bytes, 0, bytes.length);
    }

    private byte[] decodeSymmetric(final byte[] bytes, final IvParameterSpec ivParameterSpec)
            throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.DECRYPT_MODE, key, ivParameterSpec);
        return cipherInit.doFinal(bytes, AES.Const.IV_BYTES_CTR, (bytes.length - AES.Const.IV_BYTES_CTR));
    }

    private byte[] decodeSymmetric(final byte[] bytes, final GCMParameterSpec gcmParameterSpec)
            throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.DECRYPT_MODE, key, gcmParameterSpec);
        return cipherInit.doFinal(bytes, AES.Const.IV_BYTES_GCM, (bytes.length - AES.Const.IV_BYTES_GCM));
    }

    private byte[] decodeAsymmetric(final byte[] bytes) throws GeneralSecurityException {
        final Cipher cipher = KeyU.getCipher(transform);
        final Cipher cipherInit = KeyU.initCipher(cipher, Cipher.DECRYPT_MODE, key, null);
        return cipherInit.doFinal(bytes, 0, bytes.length);
    }
}
