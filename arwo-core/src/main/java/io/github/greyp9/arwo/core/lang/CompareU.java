package io.github.greyp9.arwo.core.lang;

public final class CompareU {

    private CompareU() {
    }

    public static <T extends Comparable<T>> int compare(final T left, final T right) {
        int compare = 0;
        if (left == null) {
            compare = (right == null) ? 0 : -1;
        } else if (right == null) {
            compare = 1;
        } else {
            compare = left.compareTo(right);
        }
        return compare;
    }
}
