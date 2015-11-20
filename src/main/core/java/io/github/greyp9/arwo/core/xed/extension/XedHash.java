package io.github.greyp9.arwo.core.xed.extension;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

public final class XedHash {

    private XedHash() {
    }

    public static boolean isHash(final TypeInstance typeInstance) {
        return (typeInstance.getDirective(XsdU.Xed.HASH) != null);
    }

    public static String getHash(
            final TypeInstance childInstance, final String value, final ValueInstance valueInstanceIn) {
        final TypeInstance typeInstance = valueInstanceIn.getTypeInstance();
        final String salt = childInstance.getDirective(XsdU.Xed.SALT);
        final TypeInstance typeInstanceSalt = ((salt == null) ? null : typeInstance.getInstance(salt));
        final String id = ((typeInstanceSalt == null) ? null : typeInstanceSalt.getID(typeInstance));
        final String input = ((typeInstanceSalt == null) ? value :
                (value + valueInstanceIn.getNameTypeValues().getValue(id)));
        return Base64Codec.encode(HashU.sha256(UTF8Codec.toBytes(input)));
    }
}
