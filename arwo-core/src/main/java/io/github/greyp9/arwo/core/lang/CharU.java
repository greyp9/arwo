package io.github.greyp9.arwo.core.lang;

public final class CharU {

    private CharU() {
    }

    @SuppressWarnings("PMD.UseVarargs")
    public static char[] copy(final char[] chars) {
        return ((chars == null) ? null : extract(chars, 0, chars.length));
    }

    public static char[] extract(final char[] chars, final int offset, final int length) {
        final char[] charsExtract = new char[length];
        SystemU.arraycopy(chars, offset, charsExtract, 0, length);
        return charsExtract;
    }
}
