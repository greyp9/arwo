package io.github.greyp9.arwo.core.hash.secure;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public final class HashU {

    private HashU() {
    }

    public static byte[] md5(final byte[] value) {
        return hash(Const.MD5, value);
    }

    public static byte[] sha1(final byte[] value) {
        return hash(Const.SHA1, value);
    }

    public static byte[] sha256(final byte[] value) {
        return hash(Const.SHA256, value);
    }

    private static byte[] hash(final String algorithm, final byte[] value) {
        try {
            final MessageDigest messageDigest = MessageDigest.getInstance(algorithm);
            return messageDigest.digest(value);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    private static class Const {
        private static final String MD5 = "MD5";  // i18n JRE
        private static final String SHA1 = "SHA-1";  // i18n JRE
        private static final String SHA256 = "SHA-256";  // i18n JRE
    }
}
