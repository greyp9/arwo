package io.github.greyp9.arwo.core.codec.hex;

import javax.xml.bind.DatatypeConverter;

public final class HexCodec {

    private HexCodec() {
    }

    public static String encode(final byte[] source) {
        char[] target = new char[source.length * 2];
        int posTarget = 0;
        for (final byte b : source) {
            target[posTarget++] = Const.ALPHABET[(b >> Const.SIZE_NIBBLE) & Const.MASK_NIBBLE];
            target[posTarget++] = Const.ALPHABET[(b) & Const.MASK_NIBBLE];
        }
        return new String(target, 0, posTarget);
    }

    public static byte[] decode(final String source) {
        return DatatypeConverter.parseHexBinary(source);
    }

    public static String encode(final byte[] source, final String separator) {
        final StringBuilder buffer = new StringBuilder();
        final String hex = encode(source);
        for (int i = 0; (i < hex.length()); i += 2) {
            if (buffer.length() > 0) {
                buffer.append(separator);
            }
            buffer.append(hex.substring(i, i + 2));
        }
        return buffer.toString();
    }

    private static class Const {
        private static final char[] ALPHABET = "0123456789abcdef".toCharArray();  // i18n internal
        private static final int SIZE_NIBBLE = 4;
        private static final int MASK_NIBBLE = 0xf;
    }
}
