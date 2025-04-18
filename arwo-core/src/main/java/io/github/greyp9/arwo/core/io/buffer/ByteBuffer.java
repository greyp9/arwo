package io.github.greyp9.arwo.core.io.buffer;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class ByteBuffer {
    private final Charset charset;
    private final Collection<byte[]> byteBuffers;

    public ByteBuffer(final Charset charset) {
        this.charset = charset;
        this.byteBuffers = new ArrayList<byte[]>();
    }

    public final Charset getCharset() {
        return charset;
    }

    public final synchronized int getLength() {
        int length = 0;
        for (final byte[] byteBuffer : byteBuffers) {
            length += byteBuffer.length;
        }
        return length;
    }

    public final synchronized byte[] getBytes() throws IOException {
        return getBytes(false);
    }

    public final synchronized byte[] getBytes(final boolean reset) throws IOException {
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        for (final byte[] byteBuffer : byteBuffers) {
            os.write(byteBuffer);
        }
        if (reset) {
            reset();
        }
        return os.toByteArray();
    }

    public final synchronized void addBytes(final byte[] bytes) {
        byteBuffers.add(bytes);
    }

    public final synchronized void addString(final String string) {
        byteBuffers.add(string.getBytes(charset));
    }

    public final synchronized void reset() {
        byteBuffers.clear();
    }
}
