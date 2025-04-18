package io.github.greyp9.arwo.core.io.script;

import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.command.CommandDone;
import io.github.greyp9.arwo.core.io.command.CommandToDo;
import io.github.greyp9.arwo.core.io.command.CommandWork;
import io.github.greyp9.arwo.core.text.line.LineU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Optional;
import java.util.Properties;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public final class Script {
    private final String context;
    private final long date;
    private final String id;
    private final String text;
    private final Properties properties;
    private final Collection<CommandToDo> commandsToDo;
    private final Collection<CommandWork> commandsWork;
    private final Collection<CommandDone> commandsDone;

    public String getContext() {
        return context;
    }

    public Date getDate() {
        return new Date(date);
    }

    public String getID() {
        return id;
    }

    public String getText() {
        return text;
    }

    public boolean isReload() {
        return properties.containsKey(Const.RELOAD);
    }

    public Properties getProperties() {
        return properties;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public Script(final String context, final Date date, final String id, final String text) throws IOException {
        this.context = context;
        this.date = date.getTime();
        this.id = id;
        this.text = text;
        this.properties = new Properties();
        this.commandsToDo = new ArrayList<CommandToDo>();
        this.commandsWork = new ArrayList<CommandWork>();
        this.commandsDone = new ArrayList<CommandDone>();
        final Collection<String> stdinLines = LineU.toLines(text);
        for (final String stdin : stdinLines) {
            commandsToDo.add(new CommandToDo(date, stdin));
        }
    }

    public synchronized boolean isInterrupted() {
        return PropertiesU.isBoolean(properties, Const.INTERRUPTED);
    }

    public synchronized void setInterrupted() {
        PropertiesU.setProperty(properties, Const.INTERRUPTED, Boolean.TRUE.toString());
    }

    public synchronized Date getStart() {
        final long start = new PropertiesX(properties).getLong(Const.START);
        return ((start == 0) ? null : new Date(start));
    }

    public synchronized Date getFinish() {
        final long finish = new PropertiesX(properties).getLong(Const.FINISH);
        return ((finish == 0) ? null : new Date(finish));
    }

    public synchronized void cancel() {
        final long time = new Date().getTime();
        if (!properties.containsKey(Const.START)) {
            new PropertiesX(properties).setLong(Const.START, time);
            new PropertiesX(properties).setLong(Const.FINISH, time);
        }
    }

    public synchronized void start() {
        new PropertiesX(properties).setLong(Const.START, new Date().getTime());
    }

    public synchronized void finish() {
        new PropertiesX(properties).setLong(Const.FINISH, new Date().getTime());
    }

    public synchronized void deserialize(final Date start, final Date finish) {
        final PropertiesX propertiesX = new PropertiesX(properties);
        propertiesX.setLong(Const.START, start.getTime());
        Optional.ofNullable(finish).ifPresent(f -> propertiesX.setLong(Const.FINISH, f.getTime()));
        propertiesX.setLong(Const.RELOAD, 1L);
    }

    public synchronized Collection<Command> getCommands() {
        final Collection<Command> commands = new ArrayList<Command>();
        commands.addAll(commandsDone);
        commands.addAll(commandsWork);
        commands.addAll(commandsToDo);
        return commands;
    }

    public synchronized CommandToDo getCommandToDo() {
        return (commandsToDo.isEmpty() ? null : commandsToDo.iterator().next());
    }

    public synchronized CommandWork startCommand(
            final CommandToDo command, final Charset charset, final String dir) {
        final CommandWork commandWork = new CommandWork(
                dir, command.getStdin(), charset, getID(), command.getScheduled(), new Date(), null);
        commandsToDo.remove(command);
        commandsWork.add(commandWork);
        return commandWork;
    }

    public synchronized CommandWork updateCommand(final CommandWork command, final Long pid) {
        final CommandWork commandUpdate = new CommandWork(command, pid);
        commandsWork.remove(command);
        commandsWork.add(commandUpdate);
        return commandUpdate;
    }

    public synchronized CommandDone finishCommand(final CommandWork command, final Integer exitValue) {
        final CommandDone commandDone = new CommandDone(
                command, command.getStdout(), command.getStderr(), new Date(), exitValue);
        commandsWork.remove(command);
        commandsDone.add(commandDone);
        return commandDone;
    }

    public synchronized Long getDelay(final Date now) {  // how long waiting to start
        final Date dateA = new Date(date);
        final Date dateZ = ((getStart() == null) ? now : getStart());
        final boolean isValue = (dateZ != null);
        return (isValue ? (dateZ.getTime() - dateA.getTime()) : null);
    }

    public synchronized Long getElapsed(final Date now) {
        final Date dateA = getStart();
        final Date dateZ = ((getFinish() == null) ? now : getFinish());
        final boolean isValue = ((dateA != null) && (dateZ != null));
        return (isValue ? (dateZ.getTime() - dateA.getTime()) : null);
    }

    public synchronized boolean isProgress() {
        return ((!commandsWork.isEmpty()) || (!commandsDone.isEmpty()));
    }

    public synchronized Long getStdoutLength() {
        long length = 0L;
        final Collection<Command> commands = getCommands();
        for (final Command command : commands) {
            final String stdout = command.getStdout();
            length += ((stdout == null) ? 0L : stdout.length());
        }
        return length;
    }

    public synchronized Long getStderrLength() {
        long length = 0L;
        final Collection<Command> commands = getCommands();
        for (final Command command : commands) {
            final String stderr = command.getStderr();
            length += ((stderr == null) ? 0L : stderr.length());
        }
        return length;
    }

    public synchronized Integer getExitValue() {
        Integer exitValue = 0;
        final Collection<Command> commands = getCommands();
        for (final Command command : commands) {
            exitValue = command.getExitValue();
            if (exitValue == null) {
                break;
            } else if (!exitValue.equals(0)) {
                break;
            }
        }
        return exitValue;
    }

    private static class Const {
        private static final String START = "start";  // i18n internal
        private static final String FINISH = "finish";  // i18n internal
        private static final String INTERRUPTED = "interrupted";  // i18n internal
        private static final String RELOAD = "reload";  // i18n internal
    }
}
