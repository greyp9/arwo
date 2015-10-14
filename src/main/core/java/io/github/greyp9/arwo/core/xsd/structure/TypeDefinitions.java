package io.github.greyp9.arwo.core.xsd.structure;

import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;

import java.util.Map;
import java.util.TreeMap;

public class TypeDefinitions {
    private final TypeComponents typeComponents;
    private final Map<String, DataType> simpleTypes;
    private final Map<String, DataType> complexTypes;
    private final Map<String, TypeInstance> elementTypes;

    public final TypeComponents getTypeComponents() {
        return typeComponents;
    }

    public final Map<String, DataType> getSimpleTypes() {
        return simpleTypes;
    }

    public final Map<String, DataType> getComplexTypes() {
        return complexTypes;
    }

    public final Map<String, TypeInstance> getElementTypes() {
        return elementTypes;
    }

    public TypeDefinitions(final TypeComponents typeComponents) {
        this.typeComponents = typeComponents;
        this.simpleTypes = new TreeMap<String, DataType>();
        this.complexTypes = new TreeMap<String, DataType>();
        this.elementTypes = new TreeMap<String, TypeInstance>();
    }
}
