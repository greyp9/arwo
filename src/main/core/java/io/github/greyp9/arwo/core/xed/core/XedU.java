package io.github.greyp9.arwo.core.xed.core;

// i18nf
public final class XedU {
    // namespace prefix
    //public static final String NS_PREFIX_XED = "xed";

    // namespace uri
    public static final String NS_URI_XED = "urn:xed:xed";

    // element names

    // attribute names
    public static final String ALGORITHM = "algorithm";
    public static final String HASH = "hash";
    public static final String HIDE_NAME = "hideName";
    public static final String IDENTITY = "identity";
    public static final String ITERATIONS = "iterations";
    public static final String KEYSIZE = "keysize";
    //public static final String KEYSPEC = "keyspec";
    public static final String PBE = "pbe";
    public static final String SALT = "salt";
    public static final String TRANSFORM = "transform";

    // value names

    private XedU() {
    }

    public static class Entry {
        public static final String TITLE = "title";
        public static final String CONTEXT = "context";
        public static final String QNAME = "qname";
        public static final String XML = "xml";
        public static final String XSD = "xsd";
        public static final String XSLT = "xslt";
        public static final String TRIGGER = "trigger";
    }
}
