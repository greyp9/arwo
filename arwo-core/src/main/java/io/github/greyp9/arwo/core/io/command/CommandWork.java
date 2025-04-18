package io.github.greyp9.arwo.core.io.command;

import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Date;

public class CommandWork extends Command {
    private final String dir;
    private final String stdin;
    private final String name;
    private final long dateStart;
    private final Long pid;
    private final ByteBuffer byteBufferStdin;
    private final ByteBuffer byteBufferStdout;
    private final ByteBuffer byteBufferStderr;

    @Override
    public final String getDir() {
        return dir;
    }

    @Override
    public final String getStdin() {
        return stdin;
    }

    @Override
    public final String getStdout() {
        try {
            return new String(byteBufferStdout.getBytes(), byteBufferStdout.getCharset());
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public final String getStderr() {
        try {
            return new String(byteBufferStderr.getBytes(), byteBufferStderr.getCharset());
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

    public final String getName() {
        return name;
    }

    @Override
    public final Date getStart() {
        return new Date(dateStart);
    }

    @Override
    public final Date getFinish() {
        return null;
    }

    @Override
    public final Long getPID() {
        return pid;
    }

    @Override
    public final Integer getExitValue() {
        return null;
    }

    public final ByteBuffer getByteBufferStdin() {
        return byteBufferStdin;
    }

    public final ByteBuffer getByteBufferStdout() {
        return byteBufferStdout;
    }

    public final ByteBuffer getByteBufferStderr() {
        return byteBufferStderr;
    }

    public CommandWork(final String dir, final String stdin, final Charset charset,
                       final String name, final Date dateScheduled, final Date dateStart, final Long pid) {
        super(dateScheduled);
        this.dir = dir;
        this.stdin = stdin;
        this.name = name;
        this.dateStart = dateStart.getTime();
        this.pid = pid;
        this.byteBufferStdin = new ByteBuffer(charset);
        this.byteBufferStdout = new ByteBuffer(charset);
        this.byteBufferStderr = new ByteBuffer(charset);
    }

    public CommandWork(final CommandWork command, final Long pid) {
        super(command.getScheduled());
        this.stdin = command.getStdin();
        this.dir = command.getDir();
        this.name = command.getName();
        this.dateStart = command.getStart().getTime();
        this.pid = pid;
        this.byteBufferStdin = command.getByteBufferStdin();
        this.byteBufferStdout = command.getByteBufferStdout();
        this.byteBufferStderr = command.getByteBufferStderr();
    }
}
