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
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Collection;
import java.util.Date;
import java.util.logging.Logger;

public class LocalCommandTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testPreProcess() throws Exception {
        final Script script = new Script(null, new Date(), null, "ls\npwd\ndf\n");
        final int sizeExpected = 3;
        final Collection<Command> commands = script.getCommands();
        Assert.assertEquals(sizeExpected, commands.size());
    }

    @Test
    public void testSimpleCommandGood() throws Exception {
        final Script script = new Script(null, new Date(), null, "ls");  // "cmd /c dir"
        final Collection<Command> commandsPre = script.getCommands();
        Assert.assertEquals(1, commandsPre.size());
        final AppPrincipal principal = new AppPrincipal("root", CollectionU.toCollection("*"));
        final UserExecutor executor = new UserExecutor(principal, new Date(), null);
        final ScriptContext context = new ScriptContext(
                executor.getExecutorStream(), ResultsContext.createEmpty(), null);
        final ScriptRunnable scriptRunnable = new ScriptRunnable(script, context);
        scriptRunnable.run();
        final Collection<Command> commands = script.getCommands();
        for (final Command command : commands) {
            Assert.assertTrue(command instanceof CommandDone);
            Assert.assertTrue(Value.defaultOnNull(command.getPID(), 0) > 0);
            Assert.assertEquals(Integer.valueOf(0), command.getExitValue());
            Assert.assertTrue(command.getStdin().length() > 0);
            Assert.assertTrue(command.getStdout().length() > 0);
            Assert.assertTrue(command.getStderr().length() == 0);
            final Long elapsed = DurationU.toDuration(command.getStart(), command.getFinish(), null);
            Assert.assertNotNull(elapsed);
            Assert.assertTrue(elapsed > 0);
            logger.info(command.getStdin());
            logger.info(command.getStdout());
            logger.info("" + elapsed);
        }
    }

    @Test
    public void testSimpleCommandBad() throws Exception {
        final Script script = new Script(null, new Date(), null, "lslsls");  // "dirdirdir"
        final Collection<Command> commandsPre = script.getCommands();
        Assert.assertEquals(1, commandsPre.size());
        final AppPrincipal principal = new AppPrincipal("root", CollectionU.toCollection("*"));
        final UserExecutor executor = new UserExecutor(principal, new Date(), null);
        final ScriptContext context = new ScriptContext(
                executor.getExecutorStream(), ResultsContext.createEmpty(), null);
        final ScriptRunnable scriptRunnable = new ScriptRunnable(script, context);
        scriptRunnable.run();
        final Collection<Command> commands = script.getCommands();
        for (final Command command : commands) {
            Assert.assertTrue(command instanceof CommandDone);
            //Assert.assertNull(command.getPID());  // OS difference
            //Assert.assertNull(command.getExitValue());  // OS difference
            Assert.assertTrue(command.getStdin().length() > 0);
            Assert.assertTrue(command.getStdout().length() == 0);
            Assert.assertTrue(command.getStderr().length() > 0);
            final Long elapsed = DurationU.toDuration(command.getStart(), command.getFinish(), null);
            Assert.assertNotNull(elapsed);
            Assert.assertTrue(elapsed >= 0);
            logger.info(command.getStdin());
            logger.info(command.getStderr());
            logger.info("" + elapsed);
        }
    }
}
