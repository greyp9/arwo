package io.github.greyp9.arwo.core.xed.extension;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.xed.core.XedU;
import io.github.greyp9.arwo.core.xed.transform.TransformContext;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

public final class XedHash {

    private XedHash() {
    }

    public static boolean isHash(final TypeInstance typeInstance) {
        return (typeInstance.getDirective(XedU.HASH) != null);
    }

    public static String getHash(
            final TypeInstance childInstance, final String value, final TransformContext context) {
        final StringBuilder input = new StringBuilder(value);
        final String xpathSalt = childInstance.getDirective(XedU.SALT);
        final XPather xpather = (context == null) ? null : context.getXPather();
        if ((xpathSalt != null) && (xpather != null)) {
            final String salt = xpather.getText(xpathSalt, null);
            if (salt != null) {
                input.append(salt);
            }
        }
        return Base64Codec.encode(HashU.sha256(UTF8Codec.toBytes(input.toString())));
    }
}
