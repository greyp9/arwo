package io.github.greyp9.arwo.core.io;

import io.github.greyp9.arwo.core.lang.SystemU;

public final class ByteU {

    private ByteU() {
    }

    public static byte[] copy(final byte[] bytes) {
        return extract(bytes, 0, bytes.length);
    }

    public static byte[] extract(final byte[] bytes, final int offset, final int length) {
        final byte[] bytesExtract = new byte[length];
        SystemU.arraycopy(bytes, offset, bytesExtract, 0, length);
        return bytesExtract;
    }
}