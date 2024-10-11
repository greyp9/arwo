package io.github.greyp9.arwo.core.app;

import java.util.Locale;
import java.util.ResourceBundle;

public class AppText {
    private final Locale locale;

    public AppText(final Locale locale) {
        this.locale = locale;
    }

    public final ResourceBundle getBundleCore() {
        return ResourceBundle.getBundle(App.Bundle.CORE, locale);
    }
}
