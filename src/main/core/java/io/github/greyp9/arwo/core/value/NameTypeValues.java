package io.github.greyp9.arwo.core.value;

import java.util.ArrayList;
import java.util.Collection;

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

    public NameTypeValues addNN(final String name, final Object value) {
        if ((name != null) && (value != null)) {
            add(name, value);
        }
        return this;
    }

    public NameTypeValues add(final String name, final Object value) {
        add(new NameTypeValue(name, value));
        return this;
    }

    public String getValue(final String name) {
        final NameTypeValue nameTypeValue = getNameValue(name);
        return ((nameTypeValue == null) ? null : nameTypeValue.getValueS());
    }

    public String getValueIC(final String name) {
        final NameTypeValue nameTypeValue = getNameValueIC(name);
        return ((nameTypeValue == null) ? null : nameTypeValue.getValueS());
    }

    public Collection<String> getNames() {
        final Collection<String> names = new ArrayList<String>();
        for (final NameTypeValue nameTypeValueIt : this) {
            if (!names.contains(nameTypeValueIt.getName())) {
                names.add(nameTypeValueIt.getName());
            }
        }
        return names;
    }

    public Collection<String> getValues(final String name) {
        final Collection<String> values = new ArrayList<String>();
        for (final NameTypeValue nameTypeValueIt : this) {
            final String nameIt = nameTypeValueIt.getName();
            if ((nameIt != null) && (nameIt.equals(name))) {
                values.add(nameTypeValueIt.getValueS());
            }
        }
        return values;
    }

    public NameTypeValue getNameValue(final String name) {
        NameTypeValue nameTypeValue = null;
        for (final NameTypeValue nameTypeValueIt : this) {
            final String nameIt = nameTypeValueIt.getName();
            if ((nameIt != null) && (nameIt.equals(name))) {
                nameTypeValue = nameTypeValueIt;
                break;
            }
        }
        return nameTypeValue;
    }

    public NameTypeValue getNameValueIC(final String name) {
        NameTypeValue nameTypeValue = null;
        for (final NameTypeValue nameTypeValueIt : this) {
            final String nameIt = nameTypeValueIt.getName();
            if ((nameIt != null) && (nameIt.equalsIgnoreCase(name))) {
                nameTypeValue = nameTypeValueIt;
                break;
            }
        }
        return nameTypeValue;
    }
}
