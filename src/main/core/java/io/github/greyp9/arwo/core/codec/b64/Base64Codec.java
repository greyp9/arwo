package io.github.greyp9.arwo.core.codec.b64;

import java.io.IOException;

public final class Base64Codec {

    private Base64Codec() {
    }

    public static String encode(final byte[] bytes) {
        return ((bytes == null) ? null : java.util.Base64.getEncoder().encodeToString(bytes));
    }

    public static byte[] decode(final String string) throws IOException {
        return ((string == null) ? null : java.util.Base64.getDecoder().decode(string));
    }
}
