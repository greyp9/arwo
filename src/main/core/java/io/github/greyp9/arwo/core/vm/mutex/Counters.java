package io.github.greyp9.arwo.core.vm.mutex;

import java.util.Map;
import java.util.TreeMap;

public final class Counters {
    private final Map<String, Counter> entries;

    public Counters() {
        this.entries = new TreeMap<String, Counter>();
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

    public Counter add(final String name, int amount) {
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

    public static class Counter implements Comparable<Counter> {
        private final String name;
        private int value;

        public String getName() {
            return name;
        }

        public int getValue() {
            return value;
        }

        public Counter(String name, int value) {
            this.name = name;
            this.value = value;
        }

        public void add(final int value) {
            this.value += value;
        }

        @Override
        public int compareTo(Counter o) {
            return name.compareTo(o.getName());
        }
    }
}
