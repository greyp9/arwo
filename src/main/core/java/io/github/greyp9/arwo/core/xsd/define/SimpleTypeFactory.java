package io.github.greyp9.arwo.core.xsd.define;

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
            final XsdAtom restriction = simpleType.getChildren(XsdU.RESTRICTION).iterator().next();
            final DataTypeRestrictionsFactory factory = new DataTypeRestrictionsFactory(restriction);
            final DataTypeRestrictions restrictions = factory.create();
            dataType = new DataType(name, dataTypeBase, restrictions, null);
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
        final XsdAtom restriction = simpleType.getChildren(XsdU.RESTRICTION).iterator().next();
        final String baseType = ElementU.getAttribute(restriction.getElement(), XsdU.BASE);
        return XsdAtomU.getQName(baseType, simpleType);
    }
}
