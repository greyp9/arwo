package io.github.greyp9.arwo.core.hash.text;

import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.http.Http;

public final class FingerPrint {

    private FingerPrint() {
    }

    public static String toHex(final byte[] hash) {
        return HexCodec.encode(hash, Http.Token.COLON);
    }

    public static String toBase64(final byte[] hash) {
        return Base64Codec.encode(hash);
    }

    public static String toHex256(final byte[] bytes) {
        return HexCodec.encode(HashU.sha256(bytes));
    }
}
