package io.github.greyp9.arwo.core.http;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;

import java.io.UnsupportedEncodingException;

public final class HttpArguments {

    private HttpArguments() {
    }

    public static NameTypeValues toArguments(final byte[] entity) {
        return toArguments(UTF8Codec.toString(entity));
    }

    public static NameTypeValues toArguments(final String queryString) {
        final NameTypeValues nameTypeValues = new NameTypeValues();
        final String[] queryTokens = ((queryString == null)
                ? new String[0] : queryString.split(Http.Token.BACKSLASH + Http.Token.AMP));
        for (final String queryToken : queryTokens) {
            if (!queryToken.isEmpty()) {
                final String queryTokenDecoded = URLCodec.decodeSafe(queryToken);
                final int indexOf = queryTokenDecoded.indexOf(Http.Token.EQUALS);
                if (indexOf >= 0) {
                    final String name = queryTokenDecoded.substring(0, indexOf);
                    final String value = queryTokenDecoded.substring(indexOf + Http.Token.EQUALS.length());
                    nameTypeValues.add(NameTypeValue.U.create(name, value));
                } else {
                    nameTypeValues.add(NameTypeValue.U.create(queryTokenDecoded, ""));
                }
            }
        }
        return nameTypeValues;
    }

    public static byte[] toEntity(final NameTypeValues httpArguments) throws UnsupportedEncodingException {
        return UTF8Codec.toBytes(toQueryString(httpArguments));
    }

    public static String toQueryString(final NameTypeValues httpArguments) throws UnsupportedEncodingException {
        return toQueryString(httpArguments, true);
    }

    public static String toQueryString(final NameTypeValues httpArguments,
                                       final boolean urlEncode) throws UnsupportedEncodingException {
        final StringBuilder buffer = new StringBuilder();
        for (final NameTypeValue httpArgument : httpArguments) {
            if (Value.isData(httpArgument.getName(), httpArgument.getValueS())) {
                buffer.append((buffer.length() == 0) ? "" : Http.Token.AMP);
                buffer.append(urlEncode ? URLCodec.encode(httpArgument.getName()) : httpArgument.getName());
                buffer.append(Http.Token.EQUALS);
                buffer.append(urlEncode ? URLCodec.encode(httpArgument.getValueS()) : httpArgument.getValueS());
            }
        }
        return buffer.toString();
    }
}
