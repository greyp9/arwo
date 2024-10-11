package io.github.greyp9.arwo.core.version;

import io.github.greyp9.arwo.core.lang.NumberU;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

public final class VersionU {

    private VersionU() {
    }

    public static int compare(final String versionL, final String versionR) {
        return compareTokens(toVersionTokens(versionL), toVersionTokens(versionR));
    }

    private static final String REGEX_VERSION_ALNUM = "[^a-zA-Z0-9]+";
    //private static final Pattern PATTERN_VERSION_ALNUM = Pattern.compile(REGEX_VERSION_ALNUM);

    private static final String REGEX_NUMBER = "\\d+";
    private static final Pattern PATTERN_NUMBER = Pattern.compile(REGEX_NUMBER);

    private static int compareTokens(final List<String> left, final List<String> right) {
        int compare = 0;
        while ((!left.isEmpty()) && (!right.isEmpty()) && (compare == 0)) {
            compare = compareToken(left.remove(0), right.remove(0));
        }
        if ((compare == 0) && (left.isEmpty() != right.isEmpty())) {
            compare = left.isEmpty() ? -1 : 1;
        }
        return compare;
    }

    private static int compareToken(final String left, final String right) {
        final int compare;
        final boolean isNumL = PATTERN_NUMBER.matcher(left).matches();
        final boolean isNumR = PATTERN_NUMBER.matcher(right).matches();
        if (isNumL && isNumR) {
            compare = NumberU.toInt(left, 0) - NumberU.toInt(right, 0);
        } else if (isNumL) {
            compare = -1;
        } else if (isNumR) {
            compare = 1;
        } else {
            compare = left.compareTo(right);
        }
        return compare;
    }

    private static List<String> toVersionTokens(final String version) {
        return new ArrayList<>(Arrays.asList(version.split(REGEX_VERSION_ALNUM)));
    }
}
