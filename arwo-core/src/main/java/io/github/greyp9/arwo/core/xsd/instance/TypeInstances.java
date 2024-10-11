package io.github.greyp9.arwo.core.xsd.instance;

import javax.xml.namespace.QName;
import java.util.ArrayList;
import java.util.Collection;

public class TypeInstances {
    private final QName nameParent;
    private final Collection<TypeInstance> instances;

    public final QName getQNameParent() {
        return nameParent;
    }

    public final Collection<TypeInstance> getTypeInstances() {
        return instances;
    }

    public TypeInstances(final QName nameParent) {
        this.nameParent = nameParent;
        this.instances = new ArrayList<TypeInstance>();
    }
}
