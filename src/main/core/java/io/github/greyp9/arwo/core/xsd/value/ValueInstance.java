package io.github.greyp9.arwo.core.xsd.value;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

public class ValueInstance {
    private final TypeInstance typeInstance;
    private final NameTypeValues nameTypeValues;

    public TypeInstance getTypeInstance() {
        return typeInstance;
    }

    public NameTypeValues getNameTypeValues() {
        return nameTypeValues;
    }

    public ValueInstance(TypeInstance typeInstance) {
        this.typeInstance = typeInstance;
        this.nameTypeValues = new NameTypeValues();
    }

    public void add(NameTypeValue nameTypeValue) {
        nameTypeValues.add(nameTypeValue);
    }
}
