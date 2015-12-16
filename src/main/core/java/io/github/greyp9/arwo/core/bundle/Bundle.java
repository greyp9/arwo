package io.github.greyp9.arwo.core.bundle;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
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

    public final String format(final String key, final Object... arguments) {
        final String pattern = getString(key);
        return MessageFormat.format(pattern, arguments);
    }

    public final String localize(final String... keys) {
        final Collection<String> values = new ArrayList<String>();
        for (final String key : keys) {
            values.add(getString(key));
        }
        return ((keys.length == 0) ? null : values.toString());
    }
}
