package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.xed.extension.XedHash;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import java.util.Collection;

public class ProtectHashTransform {
    private final ValueInstance valueInstanceIn;
    private final TypeInstance typeInstanceIn;

    public ProtectHashTransform(final ValueInstance valueInstanceIn) {
        this.valueInstanceIn = valueInstanceIn;
        this.typeInstanceIn = valueInstanceIn.getTypeInstance();
    }

    public final ValueInstance transform() {
        final ValueInstance valueInstance = new ValueInstance(valueInstanceIn);
        final Collection<TypeInstance> pageInstances = new TypeInstanceX(typeInstanceIn).getPageInstances();
        for (final TypeInstance pageInstance : pageInstances) {
            transform(valueInstance, pageInstance);
        }
        return valueInstance;
    }

    private void transform(final ValueInstance valueInstance, final TypeInstance pageInstance) {
        final NameTypeValue nameTypeValue = valueInstance.getNameTypeValue(pageInstance);
        if ((XedHash.isHash(pageInstance)) && (nameTypeValue != null)) {
            valueInstance.getNameTypeValues().remove(nameTypeValue);
            if (!Html.MASK.equals(nameTypeValue.getValueS())) {
                final String value = XedHash.getHash(pageInstance, nameTypeValue.getValueS(), valueInstance);
                valueInstance.add(new NameTypeValue(nameTypeValue.getName(), value));
            }
        }
    }
}
