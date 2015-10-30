package io.github.greyp9.arwo.core.app;

import javax.xml.namespace.QName;

public final class App {

    public static final String XSD_REALM = "io/github/greyp9/arwo/xsd/realm/realm.xsd";
    public static final String XML_REALM_EMPTY = "io/github/greyp9/arwo/xsd/realm/realm.xml";
    public static final QName QNAME_REALM = new QName("urn:arwo:realm", "realm", "realm");

    public static final String EXECUTOR_SERVICE = "javax.naming.Name-java.util.concurrent.ExecutorService";

    private App() {
    }
}
