package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import java.util.Collection;

/**
 * Insert xsd:default attributes when needed.
 */
public class DefaultTransform {

    public final ValueInstance transform(final ValueInstance valueInstanceIn) {
        final TypeInstance parentInstance = valueInstanceIn.getTypeInstance();
        final ValueInstance valueInstance = new ValueInstance(parentInstance);
        final NameTypeValues nameTypeValues = new NameTypeValues(valueInstanceIn.getNameTypeValues());
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(parentInstance).getPageInstances();
        for (final TypeInstance childInstance : typeInstances) {
            final String childInstanceID = childInstance.getID(parentInstance);
            final NameTypeValue nameTypeValue = nameTypeValues.getNameValue(childInstanceID);
            if (nameTypeValue == null) {
                if (childInstance.getDefault() != null) {
                    valueInstance.add(NameTypeValue.U.create(childInstanceID, childInstance.getDefault()));
                }
            } else {
                valueInstance.add(nameTypeValue);
            }
        }
        return valueInstance;
    }
}
