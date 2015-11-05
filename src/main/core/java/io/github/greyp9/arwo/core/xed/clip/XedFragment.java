package io.github.greyp9.arwo.core.xed.clip;

import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

public class XedFragment {
    private final TypeInstance typeInstance;
    private final String xml;

    public final TypeInstance getTypeInstance() {
        return typeInstance;
    }

    public final String getXml() {
        return xml;
    }

    public XedFragment(final TypeInstance typeInstance, final String xml) {
        this.typeInstance = typeInstance;
        this.xml = xml;
    }
}
