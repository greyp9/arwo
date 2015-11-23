package io.github.greyp9.arwo.core.xed.extension;

import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.jce.KeyU;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.xed.core.XedU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import java.io.IOException;
import java.security.GeneralSecurityException;

public final class XedKey {

    private XedKey() {
    }

    public static boolean isKey(final TypeInstance typeInstance) {
        return (typeInstance.getDirective(XedU.PBE) != null);
    }

    public static KeyX getKeyPBE(final char[] secret, final TypeInstance typeInstance) throws IOException {
        final byte[] salt = Base64Codec.decode(typeInstance.getDirective(XedU.SALT));
        final int iterations = NumberU.toInt(typeInstance.getDirective(XedU.ITERATIONS), 0);
        final int keySize = NumberU.toInt(typeInstance.getDirective(XedU.KEYSIZE), 0);
        final String pbe = typeInstance.getDirective(XedU.PBE);
        final String algorithm = typeInstance.getDirective(XedU.ALGORITHM);
        final String transform = typeInstance.getDirective(XedU.TRANSFORM);
        try {
            final String keyEncoded = KeyU.encodeSecretKey(
                    KeyU.toKeyPBE(secret, salt, iterations, keySize, pbe, algorithm));
            return new KeyX(KeyU.decodeSecretKey(keyEncoded, algorithm), transform);
        } catch (GeneralSecurityException e) {
            throw new IOException(e);
        }
    }
}
