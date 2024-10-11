package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import java.io.IOException;

public class ValueInstanceTransform {
    private final TransformContext context;

    public ValueInstanceTransform(final TransformContext context) {
        this.context = context;
    }

    public final ValueInstance transform(final ValueInstance valueInstance) throws IOException {
        final ValueInstance valueInstanceDefault = new DefaultTransform().transform(valueInstance);
        final ValueInstance valueInstanceName = new NameTransform().transform(valueInstanceDefault);
        final ValueInstance valueInstanceForm = new FormTransform().transform(valueInstanceName);
        final ValueInstance valueInstancePro1 = new ProtectHashTransform(valueInstanceForm, context).transform();
        final ValueInstance valueInstancePro2 = new ProtectKeyTransform(valueInstancePro1, context).transform();
        valueInstancePro2.getClass();  // suppress inspect warning
        return valueInstancePro2;
    }
}
