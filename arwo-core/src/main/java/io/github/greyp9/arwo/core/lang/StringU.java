package io.github.greyp9.arwo.core.lang;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class StringU {

    private StringU() {
    }

    public static int occurs(final String value, final char toFind) {
        int occurs = 0;
        for (final char c : value.toCharArray()) {
            if (c == toFind) {
                ++occurs;
            }
        }
        return occurs;
    }

    public static String create(final int size, final char character) {
        final char[] characters = new char[size];
        Arrays.fill(characters, character);
        return new String(characters);
    }

    public static String create(final int size, final String atom) {
        final StringBuilder buffer = new StringBuilder();
        for (int i = 0; (i < size); ++i) {
            buffer.append(atom);
        }
        return buffer.toString();
    }

    public static String appendToLength(final int size, final char character, final String value) {
        String valueNew;
        if (value == null) {
            valueNew = create(size, character);
        } else if (value.length() >= size) {
            valueNew = value;
        } else {
            valueNew = value.concat(create((size - value.length()), character));
        }
        return valueNew;
    }

    public static String truncateToLength(final int size, final String value) {
        return ((value == null) ? null : value.substring(0, Math.min(size, value.length())));
    }

    public static String splitToWidth(final String input, final int width, final String separator) {
        final StringBuilder buffer = new StringBuilder();
        final int length = input.length();
        for (int i = 0; (i < length); i += width) {
            final int widthIt = Math.min(width, length - i);
            buffer.append(input.substring(i, i + widthIt)).append(separator);
        }
        return buffer.toString();
    }

    public static String[] tokenize(final String input, final String regex) {
        return input.split(regex);
    }

    public static String replaceGroup(final Pattern pattern,
                                      final String input,
                                      final int group,
                                      final String replacement) {
        final Matcher matcher = pattern.matcher(input);
        return (matcher.matches())
                ? input.substring(0, matcher.start(group)) + replacement + input.substring(matcher.end(group)) : input;
    }

    public static String unescape(final String input) {
        final Pattern pattern = Pattern.compile(Const.UNICODE_CHAR);
        final LinkedList<Integer[]> matches = new LinkedList<>();
        final Matcher matcher = pattern.matcher(input);
        while (matcher.find()) {
            final int character = Integer.parseInt(matcher.group(1), NumberU.Const.RADIX_HEX);
            matches.push(new Integer[] { matcher.start(), character, matcher.end()});
        }
        String output = input;
        while (!matches.isEmpty()) {
            final Integer[] match = matches.pop();
            output = output.substring(0, match[0]) + ((char) match[1].intValue()) + output.substring(match[2]);
        }
        return output;
    }

    public static class Const {
        public static final String WHITESPACE = "\\s+";  // i18n internal
        public static final String UNICODE_CHAR = "\\\\u([0-9a-f]{4})";  // i18n internal
    }
}
