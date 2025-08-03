package io.github.greyp9.arwo.core.exec.script;

import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Date;

public final class ScriptContext {
    private final String id;
    private final String context;
    private final String command;
    private final Charset charset;
    private final ByteBuffer stdin;
    private final ByteBuffer stdout;
    private final ByteBuffer stderr;
    private final Date dateSubmit;
    private final File persistDir;

    private Date dateStart;
    private Date dateFinish;
    private Long pid;
    private Integer exitValue;

    public ScriptContext(final String context, final String command, final Date dateSubmit, final File persistDir) {
        this.id = DateX.Factory.createFilenameMilli().toString(dateSubmit);
        this.context = context;
        this.command = command;
        this.charset = StandardCharsets.UTF_8;
        this.stdin = new ByteBuffer(charset);
        this.stdout = new ByteBuffer(charset);
        this.stderr = new ByteBuffer(charset);
        this.dateSubmit = dateSubmit;
        this.persistDir = persistDir;
    }

    public String getID() {
        return id;
    }

    public String getContext() {
        return context;
    }

    public String getCommand() {
        return command;
    }

    public Charset getCharset() {
        return charset;
    }

    public ByteBuffer getStdin() {
        return stdin;
    }

    public ByteBuffer getStdout() {
        return stdout;
    }

    public ByteBuffer getStderr() {
        return stderr;
    }

    public Date getDateSubmit() {
        return dateSubmit;
    }

    public String getStdoutText() throws IOException {
        return new String(stdout.getBytes(), charset);
    }

    public String getStderrText() throws IOException {
        return new String(stderr.getBytes(), charset);
    }

    public File getPersistDir() {
        return persistDir;
    }

    public Date getDateStart() {
        return dateStart;
    }

    public void setDateStart(final Date dateStart) {
        this.dateStart = dateStart;
    }

    public Date getDateFinish() {
        return dateFinish;
    }

    public void setDateFinish(final Date dateFinish) {
        this.dateFinish = dateFinish;
    }

    public Long getPid() {
        return pid;
    }

    public void setPid(final Long pid) {
        this.pid = pid;
    }

    public Integer getExitValue() {
        return exitValue;
    }

    public void setExitValue(final Integer exitValue) {
        this.exitValue = exitValue;
    }
}
