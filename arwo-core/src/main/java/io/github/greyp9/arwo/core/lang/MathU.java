package io.github.greyp9.arwo.core.lang;

public final class MathU {

    private MathU() {
    }

    public static int bound(final int min, final int value, final int max) {
        return Math.max(Math.min(value, max), min);
    }

    public static long bound(final long min, final long value, final long max) {
        return Math.max(Math.min(value, max), min);
    }

    public static int roundUp(final int value, final int blockSize) {
        final int mod = value % blockSize;
        final int extra = ((mod == 0) ? 0 : (blockSize - mod));
        return (value + extra);
    }

    public static long roundUp(final long value, final long blockSize) {
        final long mod = value % blockSize;
        final long extra = ((mod == 0) ? 0 : (blockSize - mod));
        return (value + extra);
    }

    public static int log(final long value, final int base) {
        int l = 0;
        for (long i = value; (i >= base); i /= base) {
            ++l;
        }
        return l;
    }

    public static int log(final long value, final float base) {
        int l = 0;
        for (double i = value; (i >= base); i /= base) {
            ++l;
        }
        return l;
    }
}
