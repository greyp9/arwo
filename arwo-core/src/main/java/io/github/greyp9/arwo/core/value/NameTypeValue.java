package io.github.greyp9.arwo.core.value;

public class NameTypeValue {
    private final String name;
    private final String type;
    private final Object value;

    public NameTypeValue(final String name, final Object value) {
        this(name, null, value);
    }

    public NameTypeValue(final String name, final String type, final Object value) {
        this.name = name;
        this.type = type;
        this.value = value;
    }

    public NameTypeValue(final String name, final int type, final Object value) {
        this(name, Integer.toString(type), value);
    }

    public final String getName() {
        return name;
    }

    public final String getType() {
        return type;
    }

    public final String getValueS() {
        return ((value == null) ? null : value.toString());
    }

    public final Object getValue() {
        return value;
    }

    @Override
    public final String toString() {
        return String.format("[%s:%s]=[%s]", type, name, value);
    }

    public static final class U {
        private U() {
        }

        public static NameTypeValue create(final String name, final Object value) {
            return new NameTypeValue(name, value);
        }
    }
}
