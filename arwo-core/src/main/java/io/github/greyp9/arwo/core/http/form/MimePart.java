package io.github.greyp9.arwo.core.http.form;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MimePart {
    private final List<MimeHeader> headers;
    private final ByteArrayOutputStream body;

    public MimePart() {
        headers = new ArrayList<MimeHeader>();
        body = new ByteArrayOutputStream();
    }

    public final void addHeader(final MimeHeader header) {
        headers.add(header);
    }

    public final Iterator<MimeHeader> iterator() {
        return headers.iterator();
    }

    public final ByteArrayOutputStream getBody() {
        return body;
    }
}
