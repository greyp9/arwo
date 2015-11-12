package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

public class ValueInstanceTransform {

    public final ValueInstance transform(final ValueInstance valueInstance) {
        final ValueInstance valueInstanceName = new NameTransform().transform(valueInstance);
        final ValueInstance valueInstanceForm = new FormTransform().transform(valueInstanceName);
        valueInstanceForm.getClass();
        return valueInstanceForm;
    }
}
