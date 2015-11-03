package io.github.greyp9.arwo.core.submit;

import io.github.greyp9.arwo.core.lang.StringU;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SubmitTokenU {

    private SubmitTokenU() {
    }

    public static SubmitToken fromString(final String tokenString) {
        SubmitToken token = null;
        final Matcher matcher = Pattern.compile(Const.REGEX).matcher(tokenString);
        if (matcher.matches()) {
            int group = 0;
            final String subject = matcher.group(++group);
            final String action = matcher.group(++group);
            final String object = matcher.group(++group);
            final String object2 = matcher.group(++group);
            token = new SubmitToken(subject, action, object, object2);
        }
        return token;
    }

    private static class Const {
        private static final String REGEX_ATOM = "\\[(.*?)\\]";
        private static final String REGEX = StringU.create(4, REGEX_ATOM);
    }
}
