package io.github.greyp9.arwo.core.xsd.define;

import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdAtomU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.DataTypeRestrictions;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;

import javax.xml.namespace.QName;
import java.util.Iterator;
import java.util.Map;

public class SimpleTypeFactory {
    private final TypeComponents typeComponents;
    private final TypeDefinitions typeDefinitions;

    public SimpleTypeFactory(final TypeComponents typeComponents, final TypeDefinitions typeDefinitions) {
        this.typeComponents = typeComponents;
        this.typeDefinitions = typeDefinitions;
    }

    public final void create() {
        final Map<String, XsdAtom> simpleTypes = typeComponents.getSimpleTypes();
        for (final Map.Entry<String, XsdAtom> entry : simpleTypes.entrySet()) {
            processSimpleType(entry.getKey(), entry.getValue());
        }
    }

    private void processSimpleType(final String nameString, final XsdAtom simpleType) {
        final DataType dataTypeBase = processBaseType(simpleType);
        DataType dataType = typeDefinitions.getSimpleTypes().get(nameString);
        if (dataType == null) {
            final QName name = QNameU.getQName(nameString);
            if (simpleType.getChildren(XsdU.RESTRICTION).isEmpty()) {
                dataType = new DataType(name, dataTypeBase, null, null);
            } else {
                final XsdAtom restriction = simpleType.getChildren(XsdU.RESTRICTION).iterator().next();
                final DataTypeRestrictionsFactory factory = new DataTypeRestrictionsFactory(restriction);
                final DataTypeRestrictions restrictions = factory.create();
                dataType = new DataType(name, dataTypeBase, restrictions, null);
            }
            typeDefinitions.getSimpleTypes().put(nameString, dataType);
        }
    }

    private DataType processBaseType(final XsdAtom simpleType) {
        final QName qNameBase = getQNameBase(simpleType);
        final XsdAtom simpleTypeBase = typeComponents.getSimpleTypes().get(qNameBase.toString());
        DataType dataTypeBase = typeDefinitions.getSimpleTypes().get(qNameBase.toString());
        if (dataTypeBase == null) {
            processSimpleType(qNameBase.toString(), simpleTypeBase);
            dataTypeBase = typeDefinitions.getSimpleTypes().get(qNameBase.toString());
        }
        return dataTypeBase;
    }

    private QName getQNameBase(final XsdAtom simpleType) {
        QName qname = null;
        final Iterator<XsdAtom> iteratorR = simpleType.getChildren(XsdU.RESTRICTION).iterator();
        final Iterator<XsdAtom> iteratorU = simpleType.getChildren(XsdU.UNION).iterator();
        final Iterator<XsdAtom> iteratorL = simpleType.getChildren(XsdU.LIST).iterator();
        if (iteratorR.hasNext()) {
            final XsdAtom restriction = iteratorR.next();
            final String baseType = ElementU.getAttribute(restriction.getElement(), XsdU.BASE);
            qname = XsdAtomU.getQName(baseType, simpleType);
        } else if (iteratorU.hasNext()) {
            final XsdAtom union = iteratorU.next();
            final String memberTypes = ElementU.getAttribute(union.getElement(), XsdU.MEMBER_TYPES);
            final String[] tokens = StringU.tokenize(memberTypes, StringU.Const.WHITESPACE);
            qname = XsdAtomU.getQName(tokens[0], simpleType);
        } else if (iteratorL.hasNext()) {
            final XsdAtom list = iteratorL.next();
            final String itemType = ElementU.getAttribute(list.getElement(), XsdU.ITEM_TYPE);
            qname = XsdAtomU.getQName(itemType, simpleType);
        } else {
            throw new IllegalStateException(simpleType.toString());
        }
        return qname;
    }
}
