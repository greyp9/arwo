package io.github.greyp9.arwo.core.value;

import java.util.ArrayList;

public final class NameTypeValues extends ArrayList<NameTypeValue> {
    private static final long serialVersionUID = -1996418375827653958L;

    public String getValue(final String name) {
        String value = null;
        for (final NameTypeValue nameTypeValue : this) {
            final String nameNTV = nameTypeValue.getName();
            if (nameNTV.equals(name)) {
                value = nameTypeValue.getValueS();
                break;
            }
        }
        return value;
    }
}
