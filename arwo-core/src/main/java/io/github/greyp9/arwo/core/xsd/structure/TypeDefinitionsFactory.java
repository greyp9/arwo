package io.github.greyp9.arwo.core.xsd.structure;

import io.github.greyp9.arwo.core.xsd.core.XsdTypeU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.define.ComplexTypeFactory;
import io.github.greyp9.arwo.core.xsd.define.ElementFactory;
import io.github.greyp9.arwo.core.xsd.define.SimpleTypeFactory;
import io.github.greyp9.arwo.core.xsd.type.TypeComponents;

import javax.xml.namespace.QName;
import java.util.Map;

public class TypeDefinitionsFactory {
    private final TypeComponents typeComponents;
    private final TypeDefinitions typeDefinitions;

    public TypeDefinitionsFactory(final TypeComponents typeComponents) {
        this.typeComponents = typeComponents;
        this.typeDefinitions = new TypeDefinitions(typeComponents);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final TypeDefinitions create() {
        final Map<String, DataType> simpleTypes = typeDefinitions.getSimpleTypes();
        // load xsd base types
        for (final QName qname : XsdTypeU.Const.BUILTIN) {
            simpleTypes.put(qname.toString(), new DataType(qname, null, null, null));
        }
        // assemble model for document SimpleType definitions
        final SimpleTypeFactory simpleFactory = new SimpleTypeFactory(typeComponents, typeDefinitions);
        simpleFactory.create();
        // assemble model for document ComplexType definitions
        final ComplexTypeFactory complexFactory = new ComplexTypeFactory(typeComponents, typeDefinitions);
        complexFactory.create();
        // assemble model for document Element definitions
        final ElementFactory elementFactory = new ElementFactory(typeComponents, typeDefinitions);
        elementFactory.create();
        return typeDefinitions;
    }
}
