package io.github.greyp9.arwo.core.value;

import java.util.ArrayList;

public final class NameTypeValues extends ArrayList<NameTypeValue> {
    private static final long serialVersionUID = -1996418375827653958L;

    public NameTypeValues() {
        super();
    }

    public NameTypeValues(final NameTypeValue... nameTypeValues) {
        this();
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            add(nameTypeValue);
        }
    }

    public NameTypeValues(final NameTypeValues nameTypeValues) {
        this();
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            add(nameTypeValue);
        }
    }

    public void add(final String name, final Object value) {
        add(new NameTypeValue(name, value));
    }

    public String getValue(final String name) {
        final NameTypeValue nameTypeValue = getNameValue(name);
        return ((nameTypeValue == null) ? null : nameTypeValue.getValueS());
    }

    public String getValueIC(final String name) {
        final NameTypeValue nameTypeValue = getNameValueIC(name);
        return ((nameTypeValue == null) ? null : nameTypeValue.getValueS());
    }

    public NameTypeValue getNameValue(final String name) {
        NameTypeValue nameTypeValue = null;
        for (final NameTypeValue nameTypeValueIt : this) {
            if (nameTypeValueIt.getName().equals(name)) {
                nameTypeValue = nameTypeValueIt;
                break;
            }
        }
        return nameTypeValue;
    }

    public NameTypeValue getNameValueIC(final String name) {
        NameTypeValue nameTypeValue = null;
        for (final NameTypeValue nameTypeValueIt : this) {
            if (nameTypeValueIt.getName().equalsIgnoreCase(name)) {
                nameTypeValue = nameTypeValueIt;
                break;
            }
        }
        return nameTypeValue;
    }
}
