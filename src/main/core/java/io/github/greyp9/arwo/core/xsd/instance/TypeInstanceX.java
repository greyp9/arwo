package io.github.greyp9.arwo.core.xsd.instance;

import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.DataTypeU;

import javax.xml.namespace.QName;
import java.util.ArrayList;
import java.util.Collection;

public class TypeInstanceX {
    private final TypeInstance typeInstance;

    public TypeInstanceX(final TypeInstance typeInstance) {
        this.typeInstance = typeInstance;
    }

    public final Collection<TypeInstance> getPageInstances() {
        final Collection<TypeInstance> typeInstances = new ArrayList<TypeInstance>();
        // add type instance for base type
        addBaseType(typeInstances, typeInstance);
        // iterate through child type instances
        addChildTypes(typeInstances, typeInstance, Const.HIDE_IN_PAGE);
        return typeInstances;
    }

    public final Collection<TypeInstance> getTableInstances() {
        final Collection<TypeInstance> typeInstances = new ArrayList<TypeInstance>();
        // add type instance for base type
        addBaseType(typeInstances, typeInstance);
        // iterate through child type instances
        addChildTypes(typeInstances, typeInstance, Const.HIDE_IN_TABLE);
        return filterTable(typeInstances);
    }

    private static void addBaseType(final Collection<TypeInstance> typeInstances, final TypeInstance typeInstance) {
        final DataType baseTypeRoot = DataTypeU.getRootBaseType(typeInstance.getDataType());
        if ((baseTypeRoot != null) && (XsdU.NS_URI_XSD.equals(baseTypeRoot.getQName().getNamespaceURI()))) {
            final QName name = QNameU.getQName(typeInstance.getURI(), typeInstance.getName());
            //typeInstances.add(new ConcreteTypeInstance(null, NodeType.baseType, name, baseTypeRoot));  // WRONG
            typeInstances.add(new ConcreteTypeInstance(
                    null, TypeInstance.NodeType.baseType, name, typeInstance.getDataType()));
        }
    }

    private static void addChildTypes(
            final Collection<TypeInstance> typeInstances, final TypeInstance typeInstance, final String hide) {
        for (final TypeInstance typeInstanceIt : typeInstance.getInstances()) {
            final boolean hideType = Boolean.parseBoolean(typeInstanceIt.getDirective(hide));
            if (!hideType) {
                typeInstances.add(typeInstanceIt);
            }
        }
    }

    private static Collection<TypeInstance> filterTable(final Collection<TypeInstance> typeInstances) {
        final Collection<TypeInstance> filteredInstances = new ArrayList<TypeInstance>();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            final boolean isSimpleType = typeInstanceIt.isSimpleType();
            final boolean isMultiType = (typeInstanceIt.getMaxOccurs() > 1);
            final boolean isChoice = (typeInstanceIt instanceof ChoiceTypeInstance);
            if ((isSimpleType && (!isMultiType)) || isChoice) {
                filteredInstances.add(typeInstanceIt);
            }
        }
        return filteredInstances;
    }

    private static class Const {
        public static final String HIDE_IN_PAGE = "hideInPage";  // i18n internal
        public static final String HIDE_IN_TABLE = "hideInTable";  // i18n internal
    }
}
