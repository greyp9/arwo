package io.github.greyp9.arwo.core.xsd.instance;

import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;

import javax.xml.namespace.QName;
import java.util.Collection;

public class ChoiceTypeInstance extends TypeInstance {
    private final TypeInstances typeInstances;

    public final TypeInstances getTypeInstances() {
        return typeInstances;
    }

    public ChoiceTypeInstance(final QName nameParent, final String minOccurs, final String maxOccurs) {
        super(null, NodeType.choice, null, null, null, minOccurs, maxOccurs, null, null, null);
        this.typeInstances = new TypeInstances(nameParent);
    }

    @Override
    public final String getURI() {
        return XsdU.NS_URI_XSD;
    }

    @Override
    public final String getName() {
        final StringBuilder buffer = new StringBuilder();
        for (final TypeInstance instanceIt : typeInstances.getTypeInstances()) {
            buffer.append((buffer.length() == 0) ? "" : "/");
            buffer.append(instanceIt.getName());
        }
        return buffer.toString();
    }

    @Override
    public final QName getQName() {
        return new QName(getURI(), getName(), XsdU.NS_PREFIX_XSD);
    }

    @Override
    public final Collection<TypeInstance> getInstances() {
        return typeInstances.getTypeInstances();
    }

    @Override
    public final TypeInstance getInstance(final String name) {
        TypeInstance instance = null;
        for (final TypeInstance instanceIt : typeInstances.getTypeInstances()) {
            if (name.equals(instanceIt.getName())) {
                instance = instanceIt;
                break;
            }
        }
        return instance;
    }

    @Override
    public final boolean isSimpleType() {
        return false;
    }
}
