package io.github.greyp9.arwo.core.value;

import java.util.ArrayList;
import java.util.Properties;

public class NameValues extends ArrayList<NameValue> {
    private static final long serialVersionUID = 7966450333858445337L;

    public NameValues() {
        super();
    }

    public NameValues(final NameValue... nameValues) {
        this();
        for (final NameValue nameValue : nameValues) {
            add(nameValue);
        }
    }

    public NameValues(final NameValues nameValues) {
        this();
        for (final NameValue nameValue : nameValues) {
            add(nameValue);
        }
    }

    public final Properties toProperties() {
        final Properties properties = new Properties();
        for (final NameValue nameValue : this) {
            final boolean containsKey = properties.containsKey(nameValue.getName());
            final boolean containsValue = (nameValue.getValue() != null);
            if ((!containsKey) && (containsValue)) {
                properties.setProperty(nameValue.getName(), nameValue.getValue());
            }
        }
        return properties;
    }

    public final String getValue(final String name) {
        final NameValue nameValue = getNameValue(name);
        return ((nameValue == null) ? null : nameValue.getValue());
    }

    public final String getValueIC(final String name) {
        final NameValue nameValue = getNameValueIC(name);
        return ((nameValue == null) ? null : nameValue.getValue());
    }

    public final NameValue getNameValue(final String name) {
        NameValue nameValue = null;
        for (final NameValue nameValueIt : this) {
            if (nameValueIt.getName().equalsIgnoreCase(name)) {
                nameValue = nameValueIt;
                break;
            }
        }
        return nameValue;
    }

    public final NameValue getNameValueIC(final String name) {
        NameValue nameValue = null;
        for (final NameValue nameValueIt : this) {
            if (nameValueIt.getName().equalsIgnoreCase(name)) {
                nameValue = nameValueIt;
                break;
            }
        }
        return nameValue;
    }
}
