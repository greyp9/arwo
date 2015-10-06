package io.github.greyp9.arwo.core.charset;

import java.io.UnsupportedEncodingException;

public final class UTF8Codec {

    private UTF8Codec() {
    }

    public static byte[] toBytes(final String string, final String charset) {
        try {
            return ((string == null) ? null : string.getBytes(charset));
        } catch (UnsupportedEncodingException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public static byte[] toBytes(final String string) {
        try {
            return ((string == null) ? null : string.getBytes(Const.UTF8));
        } catch (UnsupportedEncodingException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public static String toString(final byte[] bytes, final String charset) {
        try {
            return ((bytes == null) ? null : new String(bytes, charset));
        } catch (UnsupportedEncodingException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public static String toString(final byte[] bytes) {
        return toString(bytes, Const.UTF8);
    }

    public static String toString(final byte[] bytes, final int offset, final int length) {
        try {
            return ((bytes == null) ? null : new String(bytes, offset, length, Const.UTF8));
        } catch (UnsupportedEncodingException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public static class Const {
        public static final String UTF8 = "UTF-8";
        public static final String UTF16 = "UTF-16";

        //public static final Charset CHARSET_UTF8 = Charset.forName(UTF8);
    }
}
