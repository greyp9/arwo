package io.github.greyp9.arwo.core.io.command.test;

import io.github.greyp9.arwo.core.command.local.ScriptContext;
import io.github.greyp9.arwo.core.command.local.ScriptRunnable;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.command.CommandDone;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

public class LocalCommandTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testPreProcess() throws Exception {
        final Script script = new Script(null, new Date(), null, "ls\npwd\ndf\n");
        final int sizeExpected = 3;
        final Collection<Command> commands = script.getCommands();
        Assertions.assertEquals(sizeExpected, commands.size());
    }

    @Test
    public void testSimpleCommandGood() throws Exception {
        final Script script = new Script(null, new Date(), null, "ls");  // "cmd /c dir"
        final Collection<Command> commandsPre = script.getCommands();
        Assertions.assertEquals(1, commandsPre.size());
        final AppPrincipal principal = new AppPrincipal("root", CollectionU.toCollection("*"));
        final UserExecutor executor = new UserExecutor(principal, new Date(), null);
        final ScriptContext context = new ScriptContext(
                executor.getExecutorStream(), ResultsContext.createEmpty(), null);
        final ScriptRunnable scriptRunnable = new ScriptRunnable(script, context);
        scriptRunnable.run();
        final Collection<Command> commands = script.getCommands();
        for (final Command command : commands) {
            Assertions.assertTrue(command instanceof CommandDone);
            Assertions.assertTrue(Value.defaultOnNull(command.getPID(), 0) > 0);
            Assertions.assertEquals(Integer.valueOf(0), command.getExitValue());
            Assertions.assertTrue(command.getStdin().length() > 0);
            Assertions.assertTrue(command.getStdout().length() > 0);
            Assertions.assertTrue(command.getStderr().length() == 0);
            final Long elapsed = DurationU.toDuration(command.getStart(), command.getFinish(), null);
            Assertions.assertNotNull(elapsed);
            Assertions.assertTrue(elapsed > 0);
            logger.finest(command.getStdin());
            logger.finest(command.getStdout());
            logger.finest("" + elapsed);
        }
    }

    @Test
    public void testSimpleCommandBad() throws Exception {
        final Script script = new Script(null, new Date(), null, "lslsls");  // "dirdirdir"
        final Collection<Command> commandsPre = script.getCommands();
        Assertions.assertEquals(1, commandsPre.size());
        final AppPrincipal principal = new AppPrincipal("root", CollectionU.toCollection("*"));
        final UserExecutor executor = new UserExecutor(principal, new Date(), null);
        final ScriptContext context = new ScriptContext(
                executor.getExecutorStream(), ResultsContext.createEmpty(), null);
        final ScriptRunnable scriptRunnable = new ScriptRunnable(script, context);
        scriptRunnable.run();
        final Collection<Command> commands = script.getCommands();
        for (final Command command : commands) {
            Assertions.assertTrue(command instanceof CommandDone);
            //Assertions.assertNull(command.getPID());  // OS difference
            //Assertions.assertNull(command.getExitValue());  // OS difference
            Assertions.assertTrue(command.getStdin().length() > 0);
            Assertions.assertTrue(command.getStdout().length() == 0);
            Assertions.assertTrue(command.getStderr().length() > 0);
            final Long elapsed = DurationU.toDuration(command.getStart(), command.getFinish(), null);
            Assertions.assertNotNull(elapsed);
            Assertions.assertTrue(elapsed >= 0);
            logger.finest(command.getStdin());
            logger.finest(command.getStderr());
            logger.finest("" + elapsed);
        }
    }
}
