package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.glyph.UTF16;

import javax.xml.namespace.QName;

public final class App {

    private App() {
    }

    public static class Actions {
        public static final String XSD = "io/github/greyp9/arwo/xsd/action/action.xsd";
        public static final QName QNAME_FILTER = new QName("urn:arwo:action", "filter", "action");
        public static final QName QNAME_LOCALE = new QName("urn:arwo:action", "locale", "action");
        public static final QName QNAME_SAVE = new QName("urn:arwo:action", "save", "action");
    }

    public static class Config {
        public static final String XSD = "io/github/greyp9/arwo/xsd/app/app.xsd";
        public static final QName QNAME = new QName("urn:arwo:app", "app", "app");
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
        public static final String APP_STATE = "javax.naming.Name-io.github.greyp9.arwo.app.core.state.AppState";
        public static final String EXECUTOR_SERVICE = "javax.naming.Name-java.util.concurrent.ExecutorService";
    }

    public static class CSS {
        public static final String ACTIVE = "active";
        public static final String MENU = "menu";
        public static final String MENUS = "menus";
    }

    public static class Action {
        public static final String CLOSE = "close";
        public static final String LOAD_REVISION = "loadRevision";
        public static final String MENU = "menu";
        public static final String NAVIGATE = "navigate";
        public static final String PRETTY = "pretty";
        public static final String RELOAD = "reload";
        public static final String RESET = "reset";
        public static final String SAVE = "save";
        public static final String TOGGLE = "toggle";
        public static final String VALIDATE = "validate";

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
        public static final String SESSION = "session";
        public static final String USER_STATE = "userState";
        public static final String VIEW_STATE = "viewState";
    }
}
