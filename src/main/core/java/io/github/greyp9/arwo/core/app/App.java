package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.glyph.UTF16;

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
        public static final String CREATE = "create";
        public static final String UPDATE = "update";
        public static final String DELETE = "delete";
        public static final String CLONE = "clone";
        public static final String UP = UTF16.ARROW_UP;
        public static final String DOWN = UTF16.ARROW_DOWN;

        public static final String FILL = "fill";
        public static final String PRUNE = "prune";

        public static final String CLIP_CLEAR = "clear";
        public static final String CLIP_CUT = "cut";
        public static final String CLIP_COPY = "copy";
        public static final String CLIP_PASTE = "paste";
    }

    public static class Target {
        public static final String DOCUMENT = "document";
        public static final String VIEW_STATE = "viewState";
    }
}
