package io.github.greyp9.arwo.core.number;

public final class NumberScale {

    private NumberScale() {
    }

    public static String toString(final long value) {
        int indexSuffix = 0;
        long left = value;
        long right = 0;
        while (left >= Const.KILO) {
            right = left % Const.KILO;
            left /= Const.KILO;
            ++indexSuffix;
        }
        final String scale = Const.SUFFIX[indexSuffix];
        String string;
        if (indexSuffix == 0) {
            string = Long.toString(left);
        } else if (left >= Const.SCALE_HUNDRED) {
            string = left + scale;
        } else if (left >= Const.SCALE_TEN) {
            string = String.format("%d.%d%s", left, (right * Const.SCALE_TEN / Const.KILO), scale);
        } else {
            string = String.format("%d.%02d%s", left, (right * Const.SCALE_HUNDRED / Const.KILO), scale);
        }
        return string;
    }

    private static class Const {
        private static final long SCALE_TEN = 10L;
        private static final long SCALE_HUNDRED = 100L;
        private static final long KILO = 1024L;

        private static final String[] SUFFIX = { "", "K", "M", "G", "T", "P", "E" };  // i18n
    }
}
