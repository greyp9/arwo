package io.github.greyp9.arwo.core.io.buffer;

import io.github.greyp9.arwo.core.io.StreamU;

import java.io.File;
import java.io.IOException;

public class ByteBufferFile extends ByteBuffer {
    private final File file;

    public ByteBufferFile(final File file) {
        super(null);
        this.file = file;
    }

    /**
     * @return the length of the backing File contents
     */
    @Override
    public final synchronized int getLength() {
        return (int) file.length();
    }

    /**
     * @return the contents of the backing File
     */
    @Override
    public final synchronized byte[] getBytes() throws IOException {
        return StreamU.read(file);
    }
}
