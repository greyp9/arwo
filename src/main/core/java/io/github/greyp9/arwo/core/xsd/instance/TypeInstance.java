package io.github.greyp9.arwo.core.xsd.instance;

import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitionsFactory;

import javax.xml.namespace.QName;
import java.util.Collection;

@SuppressWarnings("PMD.AbstractNaming")
public abstract class TypeInstance {  // xsd:complexType, xsd:simpleType @ xsd:element, xsd:attribute
    private final XsdAtom atom;
    private final NodeType nodeType;
    private final QName name;
    private final DataType dataType;
    private final DataType extensionType;
    private final String minOccurs;
    private final String maxOccurs;
    private final String use;
    private final String defaultValue;
    private final String identity;

    public TypeInstance(final XsdAtom atom, final NodeType nodeType, final QName name, final DataType dataType) {
        this(atom, nodeType, name, dataType, null, null, null, null, null, null);
    }

    @SuppressWarnings({ "PMD.ExcessiveParameterList", "checkstyle:parameternumber" })
    public TypeInstance(final XsdAtom atom, final NodeType nodeType, final QName name, final DataType dataType,
                        final DataType extensionType, final String minOccurs, final String maxOccurs,
                        final String use, final String defaultValue, final String identity) {
        this.atom = atom;
        this.nodeType = nodeType;
        this.name = name;
        this.dataType = dataType;
        this.extensionType = extensionType;
        this.minOccurs = minOccurs;
        this.maxOccurs = maxOccurs;
        this.use = use;
        this.defaultValue = defaultValue;
        this.identity = identity;
    }

    public final NodeType getNodeType() {
        return nodeType;
    }

    public abstract String getURI();

    public abstract String getName();

    public final QName getQName() {
        return name;
    }

    public final DataType getDataType() {
        return dataType;
    }

    public abstract Collection<TypeInstance> getInstances();

    public abstract TypeInstance getInstance(final String nameIn);

    @SuppressWarnings("unused")
    public final DataType getExtensionType() {
        return extensionType;
    }

    public final int getMinOccurs() {
        return Value.defaultOnNull(minOccurs, 1);
    }

    public final int getMaxOccurs() {
        final boolean unbounded = XsdU.UNBOUNDED.equals(maxOccurs);
        return ((unbounded) ? Integer.MAX_VALUE : Value.defaultOnNull(maxOccurs, 1));
    }

    public final String getUse() {
        return use;
    }

    public final String getDefault() {
        return defaultValue;
    }

    public final boolean isIdentity() {
        return Boolean.parseBoolean(identity);
    }

    public final boolean isSimpleType() {
        return dataType.getInstances().isEmpty();
    }

    public final boolean isSingleton() {
        return ((getMinOccurs() == 1) && (getMaxOccurs() == 1));
    }

    public final boolean isBoolean() {
        return ((dataType != null) && (TypeDefinitionsFactory.Const.BOOLEAN.equals(dataType.getQName())));
    }

    @SuppressWarnings("unused")
    public final String getDirective(final String nameIn) {
        return ((atom == null) ? null : atom.getDirectives().getValue(nameIn));
    }

    public final String getID() {
        return Value.join(XsdU.DOT, getName(), dataType.getQName().getLocalPart());
    }

    public final String getID(final TypeInstance parentInstance) {
        return ((parentInstance == null) ? getID() : getIDPrivate(parentInstance));
    }

    private String getIDPrivate(final TypeInstance parentInstance) {
        return Value.join(XsdU.DOT, parentInstance.getID(), getName());
    }

    @Override
    public final String toString() {
        return String.format("[%s][%s][%s][%s]", nodeType, name, dataType,
                ((extensionType == null) ? "" : extensionType));
    }
}
