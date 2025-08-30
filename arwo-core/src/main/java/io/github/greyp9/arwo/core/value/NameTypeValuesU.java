package io.github.greyp9.arwo.core.value;

public final class NameTypeValuesU {

    private NameTypeValuesU() {
    }

    public static NameTypeValues create(final String... args) {
        final NameTypeValues nameTypeValues = new NameTypeValues();
        for (int i = 0; (i < args.length); i += 2) {
            final boolean isValue = ((i + 1) < args.length);
            final String name = args[i];
            final String value = (isValue ? args[i + 1] : null);
            if ((name != null) && (value != null)) {
                nameTypeValues.add(NameTypeValue.U.create(name, value));
            }
        }
        return nameTypeValues;
    }

    public static NameTypeValues filterOut(final NameTypeValues nameTypeValuesIn, final String name) {
        final NameTypeValues nameTypeValues = new NameTypeValues();
        for (final NameTypeValue nameTypeValueIt : nameTypeValuesIn) {
            if (!nameTypeValueIt.getName().equals(name)) {
                nameTypeValues.add(nameTypeValueIt);
            }
        }
        return nameTypeValues;
    }
}
