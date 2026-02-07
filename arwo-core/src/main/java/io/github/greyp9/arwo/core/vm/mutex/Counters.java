package io.github.greyp9.arwo.core.vm.mutex;

import io.github.greyp9.arwo.core.value.Value;

import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.stream.Collectors;

public final class Counters {
    private final Map<String, Counter> entries;

    public Counters() {
        this.entries = new TreeMap<>();
    }

    public int getValue(final String name) {
        synchronized (entries) {
            final Counter counter = entries.get(name);
            return (counter == null) ? 0 : counter.getValues()[0];
        }
    }

    public int[] getValues(final String name) {
        synchronized (entries) {
            final Counter counter = entries.get(name);
            return (counter == null) ? null : counter.getValues();
        }
    }

    public int size() {
        synchronized (entries) {
            return entries.size();
        }
    }

    public Iterator<Counter> iterator() {
        return entries.values().iterator();
    }

    public Counter add(final String name, final Insert counterInsert) {
        synchronized (entries) {
            Counter counter = entries.get(name);
            if (counter == null) {
                counter = new Counter(name, counterInsert.create());
                entries.put(name, counter);
            } else {
                counterInsert.addTo(counter);
            }
            return counter;
        }
    }

    public Counter add(final String name, final int... amount) {
        synchronized (entries) {
            Counter counter = entries.get(name);
            if (counter == null) {
                counter = new Counter(name, amount);
                entries.put(name, counter);
            } else {
                counter.add(amount);
            }
            return counter;
        }
    }

    public Counter remove(final String name) {
        synchronized (entries) {
            return entries.remove(name);
        }
    }

    @Override
    public String toString() {
        return Value.joinCollection(",", entries.values().stream()
                .map(Counter::toString).collect(Collectors.toList()));
    }

    public static final class Counter implements Comparable<Counter> {
        private final String name;
        private final int[] values;

        public String getName() {
            return name;
        }

        public int getValue() {
            return values[0];
        }

        public int[] getValues() {
            return values;
        }

        public Counter(final String name, final int... values) {
            this.name = name;
            this.values = values;
        }

        public void add(final int... addValues) {
            final int n = Math.min(values.length, addValues.length);
            for (int i = 0; (i < n); ++i) {
                values[i] += addValues[i];
            }
        }

        @Override
        public int compareTo(final Counter o) {
            return name.compareTo(o.getName());
        }

        @Override
        public boolean equals(final Object o) {
            return ((o instanceof Counter) && compareTo((Counter) o) == 0);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name);
        }

        @Override
        public String toString() {
            return String.format("[%s]", name);
        }
    }

    public interface Insert {
        int[] create();

        void addTo(Counter counter);
    }
}
