package io.github.greyp9.arwo.core.runner;

import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.nio.charset.StandardCharsets;

public final class Command {
    private final File dir;
    private final String input;
    private final ByteBuffer bufferStdin;
    private final ByteBuffer bufferStdout;
    private final ByteBuffer bufferStderr;

    public Command(final String input) {
        this(null, input);
    }

    public Command(final File dir, final String input) {
        this.dir = Value.defaultOnNull(dir, new File(SystemU.userDir()));
        this.input = input;
        this.bufferStdin = new ByteBuffer(StandardCharsets.UTF_8);
        this.bufferStdout = new ByteBuffer(StandardCharsets.UTF_8);
        this.bufferStderr = new ByteBuffer(StandardCharsets.UTF_8);
    }

    public File getDir() {
        return dir;
    }

    public String getInput() {
        return input;
    }

    public ByteBuffer getStdin() {
        return bufferStdin;
    }

    public ByteBuffer getStdout() {
        return bufferStdout;
    }

    public ByteBuffer getStderr() {
        return bufferStderr;
    }
}
