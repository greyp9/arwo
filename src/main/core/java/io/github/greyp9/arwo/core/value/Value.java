package io.github.greyp9.arwo.core.value;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.StringTokenizer;
import java.util.function.Supplier;

public final class Value {

    private Value() {
    }

    public static boolean isEmpty(final byte[] value) {
        return ((value == null) || (value.length == 0));
    }

    public static boolean isEmpty(final String value) {
        return ((value == null) || (value.length() == 0));
    }

    public static boolean isData(final String value) {
        return (!isEmpty(value));
    }

    public static String defaultOnEmpty(final String... values) {
        String value = null;
        for (final String valueIt : values) {
            if (!isEmpty(valueIt)) {
                value = valueIt;
                break;
            }
        }
        return value;
    }

    public static String defaultOnEmpty(final String value, final String defaultValue) {
        return (isEmpty(value) ? defaultValue : value);
    }

    public static boolean equal(final Object left, final Object right) {
        return ((left == null) ? (right == null) : (left.equals(right)));
    }

    public static int defaultOnNull(final String value, final int defaultOnNull) {
        return ((value == null) ? defaultOnNull : Integer.parseInt(value));
    }

    public static <T> T defaultOnNull(final T t, final T defaultValue) {
        return ((t == null) ? defaultValue : t);
    }

/*
    public static <T> T firstNonNull(final T... values) {
        T value = null;
        for (T valueIt : values) {
            if (valueIt != null) {
                value = valueIt;
                break;
            }
        }
        return value;
    }
*/

    public static String joinCollection(final String connector, final Collection<String> values) {
        final Object[] valuesArray = values.toArray(new Object[values.size()]);
        return join(connector, valuesArray);
    }

    public static String join(final String connector, final Object... values) {
        final StringBuilder buffer = new StringBuilder();
        for (final Object value : values) {
            if (value != null) {
                buffer.append((buffer.length() == 0) ? "" : connector);
                buffer.append(value);
            }
        }
        return buffer.toString();
    }

    public static List<String> split(final String connector, final String values) {
        final List<String> valuesList = new ArrayList<>();
        if (values != null) {
            final StringTokenizer tokenizer = new StringTokenizer(values, connector, false);
            while (tokenizer.hasMoreTokens()) {
                valuesList.add(tokenizer.nextToken());
            }
        }
        return valuesList;
    }

    public static String wrap(final String beginAndEnd, final String value) {
        return wrap(beginAndEnd, beginAndEnd, value);
    }

    public static String wrap(final String begin, final String end, final String value) {
        return begin + value + end;
    }

/*
    public static String normalize(String value, int width, String delimiter) {
        StringBuilder buffer = new StringBuilder();
        while (!value.isEmpty()) {
            int length = Math.min(width, value.length());
            buffer.append(value.substring(0, length)).append(delimiter);
            value = value.substring(length);
        }
        return buffer.toString();
    }
*/

    public static boolean notEmpty(final String... strings) {
        boolean notEmpty = true;
        for (final String s : strings) {
            if ((s == null) || (s.length() == 0)) {
                notEmpty = false;
            }
        }
        return notEmpty;
    }

    public static char[] toCharArray(final String value) {
        return ((value == null) ? null : value.toCharArray());
    }

    public static String generate(final String value, final int count) {
        final StringBuilder buffer = new StringBuilder();
        for (int i = 0; (i < count); ++i) {
            buffer.append(value);
        }
        return buffer.toString();
    }

    public static <T extends Exception> void require(final boolean condition, final Supplier<T> e) throws T {
        if (!condition) {
            throw e.get();
        }
    }

    public static <T> T as(final Object o, final Class<T> clazz) {
        return Optional.of(o).filter(clazz::isInstance).map(clazz::cast).orElse(null);
    }
}
