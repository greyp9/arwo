package io.github.greyp9.arwo.core.xsd.instance;

import io.github.greyp9.arwo.core.xsd.data.NodeType;

import javax.xml.namespace.QName;
import java.util.Collection;

public class ChoiceTypeInstance extends TypeInstance {
    private final TypeInstances typeInstances;

    public ChoiceTypeInstance(final QName nameParent, final String minOccurs, final String maxOccurs) {
        super(null, NodeType.choice, null, null, null, minOccurs, maxOccurs, null, null, null);
        this.typeInstances = new TypeInstances(nameParent);
    }

    public final TypeInstances getTypeInstances() {
        return typeInstances;
    }

    public final Collection<TypeInstance> getInstancesC() {
        return typeInstances.getTypeInstances();
    }

    public final TypeInstance getInstanceC(final String nameIn) {
        TypeInstance instance = null;
        for (final TypeInstance instanceIt : typeInstances.getTypeInstances()) {
            if (nameIn.equals(instanceIt.getName())) {
                instance = instanceIt;
                break;
            }
        }
        return instance;
    }
}
