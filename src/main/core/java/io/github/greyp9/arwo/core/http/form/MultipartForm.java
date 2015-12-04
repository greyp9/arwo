package io.github.greyp9.arwo.core.http.form;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

public class MultipartForm {
    private final Collection<MimePart> mimeParts;

    public MultipartForm(final InputStream is) throws IOException {
        mimeParts = new ArrayList<MimePart>();
        decode(is);
    }

    public final Iterator<MimePart> iterator() {
        return mimeParts.iterator();
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void decode(final InputStream is) throws IOException {
        final ByteArrayOutputStream osPartDelimiter = new ByteArrayOutputStream();
        StreamU.readUntil(is, osPartDelimiter, Const.BYTES_CRLF);
        byte[] partDelimiter = osPartDelimiter.toByteArray();
        partDelimiter = Arrays.copyOf(partDelimiter, partDelimiter.length - Const.CRLF_LENGTH);
        MimePart mimePart = new MimePart();
        while (true) {
            final ByteArrayOutputStream osLine = new ByteArrayOutputStream();
            StreamU.readUntil(is, osLine, Const.BYTES_CRLF);
            final String line = UTF8Codec.toString(osLine.toByteArray());
            if (Const.CRLF.equals(line)) {
                readData(is, partDelimiter, mimePart.getBody());
                mimeParts.add(mimePart);
                mimePart = new MimePart();
            } else if (line.length() == 0) {
                break;
            } else {
                mimePart.addHeader(new MimeHeader(line.substring(0, line.length() - Const.CRLF_LENGTH)));
            }
        }
    }

    private void readData(final InputStream is, final byte[] partDelimiter, final OutputStream os) throws IOException {
        final ByteArrayOutputStream osBody = new ByteArrayOutputStream();
        StreamU.readUntil(is, osBody, partDelimiter);
        final byte[] bytes = osBody.toByteArray();
        final byte[] marker = StreamU.read(is, 2);
        if (Arrays.equals(marker, UTF8Codec.toBytes(Const.END_MARKER))) {
            StreamU.read(is, 2);
        }
        os.write(bytes, 0, bytes.length - (partDelimiter.length + Const.CRLF_LENGTH));
    }

    private static class Const {
        private static final String CRLF = Http.Token.CRLF;
        private static final byte[] BYTES_CRLF = UTF8Codec.toBytes(CRLF);
        private static final int CRLF_LENGTH = CRLF.length();

        private static final String END_MARKER = "--";
    }
}
