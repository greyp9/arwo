package io.github.greyp9.arwo.core.xsd.core;

import javax.xml.XMLConstants;

// i18nf
public final class XsdU {

    // namespace prefix
    //public static final String NS_PREFIX_XML = XMLConstants.XML_NS_PREFIX;  // "xml";
    public static final String NS_PREFIX_XSD = "xsd";
    public static final String NS_PREFIX_XMLNS = XMLConstants.XMLNS_ATTRIBUTE;  // "xmlns";
    public static final String NS_PREFIX_NULL = XMLConstants.DEFAULT_NS_PREFIX;

    // namespace uri
    public static final String NS_URI_XML = XMLConstants.XMLNS_ATTRIBUTE_NS_URI;  // "http://www.w3.org/2000/xmlns/";
    public static final String NS_URI_XSD = XMLConstants.W3C_XML_SCHEMA_NS_URI;  // "http://www.w3.org/2001/XMLSchema";
    public static final String NS_URI_NULL = XMLConstants.NULL_NS_URI;

    // element names
    public static final String ANNOTATION = "annotation";
    public static final String ANY = "any";
    public static final String ATTRIBUTE = "attribute";
    public static final String ATTRIBUTE_GROUP = "attributeGroup";
    public static final String CHOICE = "choice";
    public static final String COMPLEX_CONTENT = "complexContent";
    public static final String COMPLEX_TYPE = "complexType";
    public static final String ELEMENT = "element";
    public static final String ENUMERATION = "enumeration";
    public static final String EXTENSION = "extension";
    public static final String GROUP = "group";
    public static final String IMPORT = "import";
    public static final String INCLUDE = "include";
    public static final String KEY = "key";
    public static final String KEYREF = "keyref";
    public static final String LIST = "list";
    public static final String MAX_INCLUSIVE = "maxInclusive";
    public static final String MAX_LENGTH = "maxLength";
    public static final String MIN_INCLUSIVE = "minInclusive";
    public static final String MIN_LENGTH = "minLength";
    public static final String PATTERN = "pattern";
    public static final String RESTRICTION = "restriction";
    public static final String SCHEMA = "schema";
    public static final String SEQUENCE = "sequence";
    public static final String SIMPLE_CONTENT = "simpleContent";
    public static final String SIMPLE_TYPE = "simpleType";
    public static final String UNION = "union";
    public static final String UNIQUE = "unique";

    // attribute names
    public static final String ABSTRACT = "abstract";
    public static final String ATTRIBUTE_FORM_DEFAULT = "attributeFormDefault";  // NOPMD
    public static final String BASE = "base";
    public static final String DEFAULT = "default";
    public static final String ELEMENT_FORM_DEFAULT = "elementFormDefault";  // NOPMD
    public static final String FIXED = "fixed";
    public static final String ITEM_TYPE = "itemType";
    public static final String MAX_OCCURS = "maxOccurs";
    public static final String MEMBER_TYPES = "memberTypes";
    public static final String MIN_OCCURS = "minOccurs";
    public static final String NAME = "name";
    public static final String NAMESPACE = "namespace";
    public static final String REF = "ref";
    public static final String SCHEMA_LOCATION = "schemaLocation";
    public static final String SUBSTITUTION_GROUP = "substitutionGroup";  // NOPMD
    public static final String TARGET_NAMESPACE = "targetNamespace";
    public static final String TYPE = "type";
    public static final String USE = "use";
    public static final String VALUE = "value";
    public static final String VERSION = "version";

    // value names
    public static final String REQUIRED = "required";
    public static final String UNBOUNDED = "unbounded";

    // syntax
    public static final String COLON = ":";
    public static final String DASH = "-";
    public static final String DOT = ".";
    //public static final String UNDERSCORE = "_";

    // augment
    public static final String SCHEMAS = "schemas";

    private XsdU() {
    }
}
