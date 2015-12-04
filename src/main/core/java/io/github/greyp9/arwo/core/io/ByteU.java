package io.github.greyp9.arwo.core.io;

import io.github.greyp9.arwo.core.lang.SystemU;

import java.io.ByteArrayOutputStream;

public final class ByteU {

    private ByteU() {
    }

    public static byte[] copy(final byte[] bytes) {
        return ((bytes == null) ? null : extract(bytes, 0, bytes.length));
    }

    public static byte[] extract(final byte[] bytes, final int offset, final int length) {
        final byte[] bytesExtract = new byte[length];
        SystemU.arraycopy(bytes, offset, bytesExtract, 0, length);
        return bytesExtract;
    }

    public static byte[] fold(final byte[] bytes, final int newLength) {
        final byte[] bytesFolded = new byte[newLength];
        for (int i = 0; (i < bytes.length); ++i) {
            bytesFolded[i % newLength] ^= bytes[i];
        }
        return bytesFolded;
    }

    public static byte[] join(final byte[] left, final byte[] right) {
        final ByteArrayOutputStream os = new ByteArrayOutputStream(left.length + right.length);
        os.write(left, 0, left.length);
        os.write(right, 0, right.length);
        return os.toByteArray();
    }

    public static void shiftLeft(final byte[] match, final byte data) {
        SystemU.arraycopy(match, 1, match, 0, match.length - 1);
        match[match.length - 1] = data;
    }
}
