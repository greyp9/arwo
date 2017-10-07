package io.github.greyp9.arwo.core.httpclient;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.http.Http;

public final class HttpClientU {

    public static String toBasicAuth(final String basicAuthBase64) {
        return String.format("%s %s", Http.Realm.BASIC, basicAuthBase64);
    }

    public static String toBasicAuth(final String user, final char[] password) {
        return toBasicAuth(Base64Codec.encode(UTF8Codec.toBytes(user + Http.Token.COLON + new String(password))));
    }
}
