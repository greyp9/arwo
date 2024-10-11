package io.github.greyp9.arwo.core.vm.mutex;

import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;

public final class Counters {
    private final Map<String, Counter> entries;

    public Counters() {
        this.entries = new TreeMap<>();
    }

    public int getValue(final String name) {
        synchronized (entries) {
            final Counter counter = entries.get(name);
            return (counter == null) ? 0 : counter.getValue();
        }
    }

    public int size() {
        synchronized (entries) {
            return entries.size();
        }
    }

    public Counter add(final String name, final int amount) {
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

    public static final class Counter implements Comparable<Counter> {
        private final String name;
        private int value;

        public String getName() {
            return name;
        }

        public int getValue() {
            return value;
        }

        public Counter(final String name, final int value) {
            this.name = name;
            this.value = value;
        }

        public void add(final int addValue) {
            this.value += addValue;
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
    }
}
