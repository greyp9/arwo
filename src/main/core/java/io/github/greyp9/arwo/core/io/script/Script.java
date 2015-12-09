package io.github.greyp9.arwo.core.io.script;

import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.command.CommandDone;
import io.github.greyp9.arwo.core.io.command.CommandToDo;
import io.github.greyp9.arwo.core.io.command.CommandWork;
import io.github.greyp9.arwo.core.text.line.LineU;
import io.github.greyp9.arwo.core.util.PropertiesX;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Properties;

@SuppressWarnings("PMD.AvoidSynchronizedAtMethodLevel")
public class Script {
    private final long date;
    private final String text;
    private final Properties properties;
    private final Collection<CommandToDo> commandsToDo;
    private final Collection<CommandWork> commandsWork;
    private final Collection<CommandDone> commandsDone;

    public final Date getDate() {
        return new Date(date);
    }

    public final String getText() {
        return text;
    }

    public final Properties getProperties() {
        return properties;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public Script(final Date date, final String text) throws IOException {
        this.date = date.getTime();
        this.text = text;
        this.properties = new Properties();
        this.commandsToDo = new ArrayList<CommandToDo>();
        this.commandsWork = new ArrayList<CommandWork>();
        this.commandsDone = new ArrayList<CommandDone>();
        final Collection<String> stdinLines = LineU.toLines(text);
        for (final String stdin : stdinLines) {
            commandsToDo.add(new CommandToDo(stdin));
        }
    }

    public final synchronized Date getStart() {
        final long start = new PropertiesX(properties).getLong("start");
        return ((start == 0) ? null : new Date(start));
    }

    public final synchronized Date getFinish() {
        final long finish = new PropertiesX(properties).getLong("finish");
        return ((finish == 0) ? null : new Date(finish));
    }

    public final synchronized void start() {
        new PropertiesX(properties).setLong("start", new Date().getTime());
    }

    public final synchronized void finish() {
        new PropertiesX(properties).setLong("finish", new Date().getTime());
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

    public final synchronized CommandWork startCommand(final CommandToDo command, final String charset) {
        final CommandWork commandWork = new CommandWork(command.getStdin(), charset, new Date(), null);
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
}
