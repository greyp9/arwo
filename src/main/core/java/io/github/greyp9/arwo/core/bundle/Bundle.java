package io.github.greyp9.arwo.core.bundle;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class Bundle {
    private final ResourceBundle resourceBundle;

    public Bundle(final ResourceBundle resourceBundle) {
        this.resourceBundle = resourceBundle;
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public final String getString(final String key) {
        try {
            final String label = (resourceBundle == null) ? null : resourceBundle.getString(key);
            return ((label == null) ? key : label);
        } catch (MissingResourceException e) {
            return key;
        }
    }

    public final String getString(final String key, final String defaultValue) {
        final String value = getString(key);
        return (key.equals(value) ? defaultValue : value);
    }
}
