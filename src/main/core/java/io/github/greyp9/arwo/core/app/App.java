package io.github.greyp9.arwo.core.app;

import javax.xml.namespace.QName;

public final class App {

    private App() {
    }

    public static class Realm {
        public static final String XSD = "io/github/greyp9/arwo/xsd/realm/realm.xsd";
        public static final String XML_EMPTY = "io/github/greyp9/arwo/xsd/realm/realm.xml";
        public static final QName QNAME = new QName("urn:arwo:realm", "realm", "realm");
    }

    public static class Bundle {
        public static final String CORE = "io.github.greyp9.arwo.text.core.core";
    }

    public static class Naming {
        public static final String EXECUTOR_SERVICE = "javax.naming.Name-java.util.concurrent.ExecutorService";
    }

    public static class Action {
    }

    public static class Target {
        public static final String VIEW_STATE = "viewState";
    }
}
