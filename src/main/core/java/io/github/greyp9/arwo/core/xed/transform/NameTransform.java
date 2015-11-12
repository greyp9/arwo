package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import java.util.Collection;

public class NameTransform {

    public final ValueInstance transform(final ValueInstance valueInstanceIn) {
        final TypeInstance parentInstance = valueInstanceIn.getTypeInstance();
        final ValueInstance valueInstance = new ValueInstance(parentInstance);
        final NameTypeValues nameTypeValues = new NameTypeValues(valueInstanceIn.getNameTypeValues());
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(parentInstance).getPageInstances();
        for (final TypeInstance childInstance : typeInstances) {
            final String name = childInstance.getName();
            final NameTypeValue nameTypeValue = nameTypeValues.getNameValue(name);
            if (nameTypeValue != null) {
                final String nameFull = childInstance.getID(parentInstance);
                valueInstance.add(NameTypeValue.U.create(nameFull, nameTypeValue.getValue()));
                nameTypeValues.remove(nameTypeValue);
            }
        }
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            valueInstance.add(nameTypeValue);
        }
        return valueInstance;
    }
}
