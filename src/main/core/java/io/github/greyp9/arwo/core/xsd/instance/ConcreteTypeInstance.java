package io.github.greyp9.arwo.core.xsd.instance;

import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.NodeType;

import javax.xml.namespace.QName;
import java.util.Collection;

public class ConcreteTypeInstance extends TypeInstance {

    public ConcreteTypeInstance(
            final XsdAtom atom, final NodeType nodeType, final QName name, final DataType dataType) {
        super(atom, nodeType, name, dataType, null, null, null, null, null, null);
    }

    @SuppressWarnings({ "PMD.ExcessiveParameterList", "checkstyle:parameternumber" })
    public ConcreteTypeInstance(final XsdAtom atom, final NodeType nodeType, final QName name, final DataType dataType,
                                final DataType extensionType, final String minOccurs, final String maxOccurs,
                                final String use, final String defaultValue, final String identity) {
        super(atom, nodeType, name, dataType, extensionType, minOccurs, maxOccurs, use, defaultValue, identity);
    }

    @Override
    public final String getURI() {
        final QName qname = super.getQName();
        return ((qname == null) ? null : qname.getNamespaceURI());
    }

    @Override
    public final String getName() {
        final QName qname = super.getQName();
        return ((qname == null) ? null : qname.getLocalPart());
    }

    @Override
    public final Collection<TypeInstance> getInstances() {
        return super.getDataType().getInstances();
    }

    @Override
    public final TypeInstance getInstance(final String name) {
        return super.getDataType().getInstance(name);
    }
}
