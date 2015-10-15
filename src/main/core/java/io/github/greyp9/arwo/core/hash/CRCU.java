package io.github.greyp9.arwo.core.hash;

import io.github.greyp9.arwo.core.lang.NumberU;

import java.util.zip.CRC32;

public final class CRCU {

    private CRCU() {
    }

    public static long crc32(final byte[] value) {
        final CRC32 crc32 = new CRC32();
        crc32.update(value);
        return crc32.getValue();
    }

    public static String crc32String(final byte[] value) {
        return NumberU.toHex((int) crc32(value));

    }
}
