package io.github.greyp9.arwo.core.bundle;

import java.util.ResourceBundle;

public class Bundle {
    private final ResourceBundle[] resourceBundles;

    public Bundle(final ResourceBundle... resourceBundles) {
        this.resourceBundles = resourceBundles;
    }

    public final String getString(final String key) {
        String value = key;
        for (final ResourceBundle resourceBundle : resourceBundles) {
            if (resourceBundle.containsKey(key)) {
                value = resourceBundle.getString(key);
                break;
            }
        }
        return value;
    }

    public final String getString(final String key, final String defaultValue) {
        final String value = getString(key);
        return (key.equals(value) ? defaultValue : value);
    }
}