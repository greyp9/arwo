package io.github.greyp9.arwo.core.lang;

public final class TypeU {

    private TypeU() {
    }

    public static boolean toBooleanP(final String value) {
        final Boolean b = toBoolean(value);
        return ((b != null) && (b));
    }

    public static Boolean toBoolean(final String value) {
        return ((value == null) ? null : Boolean.valueOf(value));
    }

    public static Integer toInteger(final String value) {
        return ((value == null) ? null : Integer.valueOf(value));
    }

    public static Long toLong(final String value) {
        return ((value == null) ? null : Long.valueOf(value));
    }

    public static String toString(final Boolean value) {
        return ((value == null) ? null : Boolean.toString(value));
    }

    public static String toString(final Integer value) {
        return ((value == null) ? null : Integer.toString(value));
    }

    public static String toString(final Long value) {
        return ((value == null) ? null : Long.toString(value));
    }
}
