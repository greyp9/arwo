package io.github.greyp9.arwo.core.hash;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.lang.NumberU;

import java.util.zip.CRC32;

public final class CRCU {

    private CRCU() {
    }

    public static long crc32(final byte[] bytes) {
        return crc32(bytes, 0, bytes.length);
    }

    public static long crc32(final byte[] bytes, final int offset, final int length) {
        final CRC32 crc32 = new CRC32();
        crc32.update(bytes, offset, length);
        return crc32.getValue();
    }

    public static String crc32String(final String value) {
        return crc32String(UTF8Codec.toBytes(value));
    }

    public static String crc32String(final byte[] value) {
        return NumberU.toHex((int) crc32(value));
    }
}
