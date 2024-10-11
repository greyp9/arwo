package io.github.greyp9.arwo.core.codec.gz;

import io.github.greyp9.arwo.core.io.StreamU;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

public class GZipCodec {

    public final byte[] encode(final byte[] bytes) throws IOException {
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        final GZIPOutputStream gos = new GZIPOutputStream(bos);
        gos.write(bytes);
        gos.finish();
        return bos.toByteArray();
    }

    public final byte[] decode(final byte[] bytes) throws IOException {
        final ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
        final GZIPInputStream gis = new GZIPInputStream(bis);
        return StreamU.read(gis);
    }
}
