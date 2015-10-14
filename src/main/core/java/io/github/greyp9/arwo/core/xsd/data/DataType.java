package io.github.greyp9.arwo.core.xsd.data;

import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import javax.xml.namespace.QName;
import java.util.ArrayList;
import java.util.Collection;

public class DataType {
    private final QName name;
    private final DataType baseType;
    private final DataTypeRestrictions restrictions;
    private final Collection<TypeInstance> instances;

    public final QName getQName() {
        return name;
    }

    public final DataType getBaseType() {
        return baseType;
    }

    public final DataTypeRestrictions getRestrictions() {
        return restrictions;
    }

    public final String getDefaultEnumValue() {
        final Collection<String> enumValues = restrictions.getEnumValues();
        return (enumValues.isEmpty() ? null : enumValues.iterator().next());
    }

    public final Collection<TypeInstance> getInstances() {
        return instances;
    }

    public DataType(final QName name, final DataType baseType,
                    final DataTypeRestrictions restrictions, final Collection<TypeInstance> instances) {
        this.name = name;
        this.baseType = baseType;
        this.restrictions = ((restrictions == null) ? new DataTypeRestrictions() : restrictions);
        this.instances = ((instances == null) ? new ArrayList<TypeInstance>() : instances);
    }

    public final TypeInstance getInstance(final String nameIn) {
        TypeInstance instance = null;
        for (final TypeInstance instanceIt : instances) {
            if (instanceIt instanceof ChoiceTypeInstance) {
                instance = instanceIt.getInstance(nameIn);
            } else if (nameIn.equals(instanceIt.getName())) {
                instance = instanceIt;
            }
            if (instance != null) {
                break;
            }
        }
        return instance;
    }

    @Override
    public final String toString() {
        return String.format("%s%s", name, ((baseType == null) ? "" : baseType.toString()));
    }
}
