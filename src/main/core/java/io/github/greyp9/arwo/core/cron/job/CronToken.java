package io.github.greyp9.arwo.core.cron.job;

import java.util.BitSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CronToken {
    private final int nbits;
    private final int offset;
    private final BitSet bitSet;

    public CronToken(final int nbits, final int offset) {
        this.nbits = nbits;
        this.offset = offset;
        this.bitSet = new BitSet(nbits);
    }

    public final BitSet apply(final String token) {
        if (Const.ALL.equals(token)) {
            bitSet.set(0, bitSet.size());
        } else {
            final String[] atoms = token.split(Const.DELIMITER);
            for (final String atom : atoms) {
                applyAtom(atom);
            }
        }
        return bitSet;
    }

    private void applyAtom(final String atom) {
        final Matcher matcherNumber = Pattern.compile(Const.PATTERN_NUMBER).matcher(atom);
        final Matcher matcherDash = Pattern.compile(Const.PATTERN_DASH).matcher(atom);
        final Matcher matcherSlash = Pattern.compile(Const.PATTERN_SLASH).matcher(atom);
        int group = 0;
        if (matcherNumber.matches()) {
            final int bitIndex = Integer.parseInt(matcherNumber.group(++group));
            setBit(bitIndex);
        } else if (matcherDash.matches()) {
            final int min = Integer.parseInt(matcherDash.group(++group));
            final int max = Integer.parseInt(matcherDash.group(++group));
            for (int bitIndex = min; (bitIndex <= max); ++bitIndex) {
                setBit(bitIndex);
            }
        } else if (matcherSlash.matches()) {
            final int interval = Integer.parseInt(matcherSlash.group(++group));
            for (int bitIndex = 0; (bitIndex < nbits); bitIndex += interval) {
                setBit(bitIndex);
            }
        }
    }

    private void setBit(final int bitIndex) {
        final int bitIndexOffset = bitIndex + offset;
        bitSet.set(bitIndexOffset % nbits);
    }

    private static class Const {
        private static final String ALL = "*";  // i18n internal
        private static final String DELIMITER = ",";  // i18n internal
        private static final String PATTERN_NUMBER = "(\\d+)";  // i18n internal
        private static final String PATTERN_DASH = "(\\d+)-(\\d+)";  // i18n internal
        private static final String PATTERN_SLASH = "\\*/(\\d+)";  // i18n internal
    }
}
