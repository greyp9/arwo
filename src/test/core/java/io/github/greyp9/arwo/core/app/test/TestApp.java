package io.github.greyp9.arwo.core.app.test;

import io.github.greyp9.arwo.core.app.App;

public final class TestApp {

    private TestApp() {
    }

    public static class Resources {
        public static final String XSD_CHOICE1 = "io/github/greyp9/arwo/xsd/choice1/choice1.xsd";
        public static final String XSD_CHOICE2 = "io/github/greyp9/arwo/xsd/choice2/choice2.xsd";
        public static final String XSD_CHOICE_NO_NS = "io/github/greyp9/arwo/xsd/choiceNoNs/choiceNoNs.xsd";
        public static final String XSD_ENUM1 = "io/github/greyp9/arwo/xsd/enum1/enum1.xsd";
        public static final String XSD_NO_NS = "io/github/greyp9/arwo/xsd/noNs/noNs.xsd";
        public static final String XSD_ORDER = "io/github/greyp9/arwo/xsd/order/order.xsd";
        public static final String XSD_PROTECT = "io/github/greyp9/arwo/xsd/protect/protect.xsd";

        public static final String[] XSD_ARRAY = { App.Realm.XSD };
    }
}
