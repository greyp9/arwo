package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.lang.CharU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.xed.extension.XedKey;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import java.io.IOException;
import java.util.Collection;

public class ProtectKeyTransform {
    private final ValueInstance valueInstanceIn;
    private final TypeInstance typeInstanceIn;
    private final char[] secret;

    @SuppressWarnings("PMD.UseVarargs")
    public ProtectKeyTransform(final ValueInstance valueInstanceIn, final char[] secret) {
        this.valueInstanceIn = valueInstanceIn;
        this.typeInstanceIn = valueInstanceIn.getTypeInstance();
        this.secret = CharU.copy(secret);
    }

    public final ValueInstance transform() throws IOException {
        final ValueInstance valueInstance = new ValueInstance(valueInstanceIn);
        final Collection<TypeInstance> pageInstances = new TypeInstanceX(typeInstanceIn).getPageInstances();
        for (final TypeInstance pageInstance : pageInstances) {
            transform(valueInstance, pageInstance);
        }
        return valueInstance;
    }

    private void transform(final ValueInstance valueInstance, final TypeInstance pageInstance) throws IOException {
        final NameTypeValue nameTypeValue = valueInstance.getNameTypeValue(pageInstance);
        if ((XedKey.isKey(pageInstance)) && (nameTypeValue != null)) {
            valueInstance.getNameTypeValues().remove(nameTypeValue);
            if (!Html.MASK.equals(nameTypeValue.getValueS())) {
                final KeyX key = XedKey.getKeyPBE(secret, pageInstance);
                final String value = key.protect(nameTypeValue.getValueS());
                valueInstance.add(new NameTypeValue(nameTypeValue.getName(), value));
            }
        }
    }
}
