package io.github.greyp9.arwo.core.codec.b64;

import javax.xml.bind.DatatypeConverter;
import java.io.IOException;

public final class Base64Codec {

    private Base64Codec() {
    }

    public static String encode(final byte[] bytes) {
        return ((bytes == null) ? null : DatatypeConverter.printBase64Binary(bytes));
    }

    public static byte[] decode(final String string) throws IOException {
        return ((string == null) ? null : DatatypeConverter.parseBase64Binary(string));
    }
}
