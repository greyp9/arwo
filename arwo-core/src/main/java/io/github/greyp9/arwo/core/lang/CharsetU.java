package io.github.greyp9.arwo.core.lang;

import java.nio.charset.Charset;

public final class CharsetU {

    private CharsetU() {
    }

    public static byte[] toBytes(final String string, final Charset charset) {
        return ((string == null) ? null : string.getBytes(charset));
    }

    public static String toString(final byte[] bytes, final Charset charset) {
        return ((bytes == null) ? null : new String(bytes, charset));
    }
}
