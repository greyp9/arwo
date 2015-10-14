package io.github.greyp9.arwo.core.xsd.structure;

import io.github.greyp9.arwo.core.xsd.core.XsdU;
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
        for (final QName qname : Const.BUILTIN) {
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

    public static class Const {
        // http://stackoverflow.com/questions/29845983/
        // default attribute type [http://www.w3.org/TR/xmlschema-1/#cAttribute_Declarations]
        public static final QName ANY_SIMPLE_TYPE = new QName(XsdU.NS_URI_XSD, "anySimpleType");
        // default element type [http://www.w3.org/TR/xmlschema-1/#Element_Declaration_details]
        public static final QName ANY_TYPE = new QName(XsdU.NS_URI_XSD, "anyType");
        public static final QName ANY_URI = new QName(XsdU.NS_URI_XSD, "anyURI");
        public static final QName BASE_64_BINARY = new QName(XsdU.NS_URI_XSD, "base64Binary");
        public static final QName BOOLEAN = new QName(XsdU.NS_URI_XSD, "boolean");
        public static final QName DATE = new QName(XsdU.NS_URI_XSD, "date");
        public static final QName DATE_TIME = new QName(XsdU.NS_URI_XSD, "dateTime");
        public static final QName DECIMAL = new QName(XsdU.NS_URI_XSD, "decimal");
        public static final QName DURATION = new QName(XsdU.NS_URI_XSD, "duration");
        public static final QName ID = new QName(XsdU.NS_URI_XSD, "ID");
        public static final QName INT = new QName(XsdU.NS_URI_XSD, "int");
        public static final QName INTEGER = new QName(XsdU.NS_URI_XSD, "integer");
        public static final QName LONG = new QName(XsdU.NS_URI_XSD, "long");
        public static final QName NON_NEG_INTEGER = new QName(XsdU.NS_URI_XSD, "nonNegativeInteger");
        public static final QName NORMALIZED_STRING = new QName(XsdU.NS_URI_XSD, "normalizedString");
        public static final QName POSITIVE_INTEGER = new QName(XsdU.NS_URI_XSD, "positiveInteger");
        public static final QName STRING = new QName(XsdU.NS_URI_XSD, "string");
        public static final QName TIME = new QName(XsdU.NS_URI_XSD, "time");
        public static final QName TOKEN = new QName(XsdU.NS_URI_XSD, "token");
        public static final QName UNSIGNED_BYTE = new QName(XsdU.NS_URI_XSD, "unsignedByte");
        public static final QName UNSIGNED_INT = new QName(XsdU.NS_URI_XSD, "unsignedInt");
        public static final QName UNSIGNED_LONG = new QName(XsdU.NS_URI_XSD, "unsignedLong");
        public static final QName UNSIGNED_SHORT = new QName(XsdU.NS_URI_XSD, "unsignedShort");

        private static final QName[] BUILTIN = {
                ANY_SIMPLE_TYPE, ANY_TYPE, ANY_URI, BASE_64_BINARY, BOOLEAN, DATE,
                DATE_TIME, DECIMAL, DURATION, ID, INT, INTEGER, LONG,
                NON_NEG_INTEGER, NORMALIZED_STRING, POSITIVE_INTEGER, STRING, TIME,
                TOKEN, UNSIGNED_BYTE, UNSIGNED_INT, UNSIGNED_LONG, UNSIGNED_SHORT,
        };
    }
}
