package io.github.greyp9.arwo.core.vm.mutex;

public final class CounterInsert implements Counters.Insert {
    private final int count;
    private final int amount;

    public CounterInsert(final int count, final int amount) {
        this.count = count;
        this.amount = amount;
    }

    @Override
    public int[] create() {
        return new int[] {count, amount, amount, amount};
    }

    @Override
    public void addTo(final Counters.Counter counter) {
        final int[] values = counter.getValues();
        values[N_COUNT] += count;
        values[N_AMOUNT] += amount;
        values[N_MIN] = Math.min(values[N_MIN], amount);
        values[N_MAX] = Math.max(values[N_MAX], amount);
    }

    private static final int N_COUNT = 0;
    private static final int N_AMOUNT = 1;
    private static final int N_MIN = 2;
    private static final int N_MAX = 3;
}
