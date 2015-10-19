package io.github.greyp9.arwo.core.bundle;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

public class Bundle {
    private final ResourceBundle bundle;

    public Bundle(ResourceBundle bundle) {
        this.bundle = bundle;
    }

    public String getString(String key) {
        try {
            String label = (bundle == null) ? null : bundle.getString(key);
            return ((label == null) ? key : label);
        } catch (MissingResourceException e) {
            return key;
        }
    }

    public String getString(String key, String defaultValue) {
        String value = getString(key);
        return (key.equals(value) ? defaultValue : value);
    }
}
