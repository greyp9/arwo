package io.github.greyp9.arwo.core.codec.hex;

public final class HexCodec {

    private HexCodec() {
    }

    public static String encode(final byte[] source) {
        char[] target = new char[source.length * 2];
        int posTarget = 0;
        for (final byte b : source) {
            target[posTarget++] = Const.CHARS[(b >> Const.SIZE_NIBBLE) & Const.MASK_NIBBLE];
            target[posTarget++] = Const.CHARS[(b) & Const.MASK_NIBBLE];
        }
        return new String(target, 0, posTarget);
    }

    public static byte[] decode(final String source) {
        if (source == null) {
            return null;
        }
        final int length = source.length();
        if ((length % 2) != 0) {
            throw new IllegalArgumentException(source);
        }
        final byte[] bytes = new byte[length / 2];
        final char[] chars = source.toCharArray();
        int posSource = -1;
        for (int posTarget = 0; (posTarget < bytes.length); ++posTarget) {
            final int highNibble = Const.ALPHABET.indexOf(chars[++posSource]);
            final int lowNibble = Const.ALPHABET.indexOf(chars[++posSource]);
            bytes[posTarget] = (byte) ((highNibble << (Byte.SIZE / 2)) + lowNibble);
        }
        return bytes;
    }

    public static String encode(final byte[] source, final String separator) {
        final StringBuilder buffer = new StringBuilder();
        final String hex = encode(source);
        for (int i = 0; (i < hex.length()); i += 2) {
            if (buffer.length() > 0) {
                buffer.append(separator);
            }
            buffer.append(hex, i, i + 2);
        }
        return buffer.toString();
    }

    private static class Const {
        private static final String ALPHABET = "0123456789abcdef";  // i18n internal
        private static final char[] CHARS = ALPHABET.toCharArray();
        private static final int SIZE_NIBBLE = 4;
        private static final int MASK_NIBBLE = 0xf;
    }
}
