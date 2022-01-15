package io.github.greyp9.arwo.core.io.script;

import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.command.CommandDone;
import io.github.greyp9.arwo.core.io.command.CommandToDo;
import io.github.greyp9.arwo.core.io.command.CommandWork;
import io.github.greyp9.arwo.core.text.line.LineU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Properties;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class Script {
    private final String context;
    private final long date;
    private final String id;
    private final String text;
    private final Properties properties;
    private final Collection<CommandToDo> commandsToDo;
    private final Collection<CommandWork> commandsWork;
    private final Collection<CommandDone> commandsDone;

    public final String getContext() {
        return context;
    }

    public final Date getDate() {
        return new Date(date);
    }

    public final String getID() {
        return id;
    }

    public final String getText() {
        return text;
    }

    public final Properties getProperties() {
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

    public final synchronized boolean isInterrupted() {
        return PropertiesU.isBoolean(properties, Const.INTERRUPTED);
    }

    public final synchronized void setInterrupted() {
        PropertiesU.setProperty(properties, Const.INTERRUPTED, Boolean.TRUE.toString());
    }

    public final synchronized Date getStart() {
        final long start = new PropertiesX(properties).getLong(Const.START);
        return ((start == 0) ? null : new Date(start));
    }

    public final synchronized Date getFinish() {
        final long finish = new PropertiesX(properties).getLong(Const.FINISH);
        return ((finish == 0) ? null : new Date(finish));
    }

    public final synchronized void start() {
        new PropertiesX(properties).setLong(Const.START, new Date().getTime());
    }

    public final synchronized void finish() {
        new PropertiesX(properties).setLong(Const.FINISH, new Date().getTime());
    }

    public final synchronized Collection<Command> getCommands() {
        final Collection<Command> commands = new ArrayList<Command>();
        commands.addAll(commandsDone);
        commands.addAll(commandsWork);
        commands.addAll(commandsToDo);
        return commands;
    }

    public final synchronized CommandToDo getCommandToDo() {
        return (commandsToDo.isEmpty() ? null : commandsToDo.iterator().next());
    }

    public final synchronized CommandWork startCommand(
            final CommandToDo command, final String charset, final String dir) {
        final CommandWork commandWork = new CommandWork(
                dir, command.getStdin(), charset, getID(), command.getScheduled(), new Date(), null);
        commandsToDo.remove(command);
        commandsWork.add(commandWork);
        return commandWork;
    }

    public final synchronized CommandWork updateCommand(final CommandWork command, final Integer pid) {
        final CommandWork commandUpdate = new CommandWork(command, pid);
        commandsWork.remove(command);
        commandsWork.add(commandUpdate);
        return commandUpdate;
    }

    public final synchronized CommandDone finishCommand(final CommandWork command, final Integer exitValue) {
        final CommandDone commandDone = new CommandDone(
                command, command.getStdout(), command.getStderr(), new Date(), exitValue);
        commandsWork.remove(command);
        commandsDone.add(commandDone);
        return commandDone;
    }

    public final synchronized Long getDelay(final Date now) {  // how long waiting to start
        final Date dateA = new Date(date);
        final Date dateZ = ((getStart() == null) ? now : getStart());
        final boolean isValue = (dateZ != null);
        return (isValue ? (dateZ.getTime() - dateA.getTime()) : null);
    }

    public final synchronized Long getElapsed(final Date now) {
        final Date dateA = getStart();
        final Date dateZ = ((getFinish() == null) ? now : getFinish());
        final boolean isValue = ((dateA != null) && (dateZ != null));
        return (isValue ? (dateZ.getTime() - dateA.getTime()) : null);
    }

    public final synchronized long getStdoutLength() {
        long length = 0L;
        final Collection<Command> commands = getCommands();
        for (final Command command : commands) {
            final String stdout = command.getStdout();
            length += ((stdout == null) ? 0L : stdout.length());
        }
        return length;
    }

    public final synchronized long getStderrLength() {
        long length = 0L;
        final Collection<Command> commands = getCommands();
        for (final Command command : commands) {
            final String stderr = command.getStderr();
            length += ((stderr == null) ? 0L : stderr.length());
        }
        return length;
    }

    public final synchronized Integer getExitValue() {
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
    }
}
