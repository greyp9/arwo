package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import java.io.IOException;

public class ValueInstanceTransform {

    public final ValueInstance transform(final ValueInstance valueInstance) throws IOException {
        return transform(valueInstance, null);
    }

    @SuppressWarnings("PMD.UseVarargs")
    public final ValueInstance transform(final ValueInstance valueInstance, final char[] secret) throws IOException {
        final ValueInstance valueInstanceName = new NameTransform().transform(valueInstance);
        final ValueInstance valueInstanceForm = new FormTransform().transform(valueInstanceName);
        final ValueInstance valueInstancePro1 = new ProtectHashTransform(valueInstanceForm).transform();
        final ValueInstance valueInstancePro2 = new ProtectKeyTransform(valueInstancePro1, secret).transform();
        valueInstancePro2.getClass();  // suppress inspect warning
        return valueInstancePro2;
    }
}
