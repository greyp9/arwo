package io.github.greyp9.arwo.core.http.mime;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

public class MimeMagic {
    private final InputStream is;

    public MimeMagic(final InputStream is) {
        this.is = is;
    }

    public final boolean isXML() throws IOException {
        final byte[] header = StreamU.read(is, Const.XML.length);
        return Arrays.equals(Const.XML, header);
    }

    private static class Const {
        private static final byte[] XML = UTF8Codec.toBytes("<?xml");
    }
}
