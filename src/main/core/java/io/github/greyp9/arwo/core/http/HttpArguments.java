package io.github.greyp9.arwo.core.http;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;

public final class HttpArguments {

    private HttpArguments() {
    }

    public static NameTypeValues toArguments(final byte[] entity) {
        return toArguments(UTF8Codec.toString(entity));
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public static NameTypeValues toArguments(final String queryString) {
        final NameTypeValues nameTypeValues = new NameTypeValues();
        final String[] queryTokens = ((queryString == null) ?
                new String[0] : queryString.split(Http.Token.BACKSLASH + Http.Token.AMP));
        for (final String queryToken : queryTokens) {
            if (queryToken.length() > 0) {
                final String queryTokenDecoded = URLCodec.decodeSafe(queryToken);
                final int indexOf = queryTokenDecoded.indexOf(Http.Token.EQUALS);
                if (indexOf >= 0) {
                    final String name = queryTokenDecoded.substring(0, indexOf);
                    final String value = queryTokenDecoded.substring(indexOf + Http.Token.EQUALS.length());
                    nameTypeValues.add(new NameTypeValue(name, value));
                } else {
                    nameTypeValues.add(new NameTypeValue(queryTokenDecoded, ""));
                }
            }
        }
        return nameTypeValues;
    }
}
