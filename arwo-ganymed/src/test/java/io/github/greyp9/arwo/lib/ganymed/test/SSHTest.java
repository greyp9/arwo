package io.github.greyp9.arwo.lib.ganymed.test;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.InteractiveCallback;
import ch.ethz.ssh2.SFTPv3Client;
import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable.ScriptContext;
import io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable.ScriptRunnable;
import io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable.ScriptX;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnectionX;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.security.GeneralSecurityException;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

public class SSHTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testServerConnectivity() throws Exception {
        final File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assumptions.assumeTrue(fileProperties.exists());
        final Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assertions.assertTrue(properties.size() > 0);
        final String sshServerList = properties.getProperty(Const.SSH_SERVER);
        Assertions.assertNotNull(sshServerList);
        final String[] servers = sshServerList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(final String server, final Properties properties) throws IOException {
        final String host = properties.getProperty(String.format("%s.%s.host", Const.SSH_SERVER, server));
        final String port = properties.getProperty(String.format("%s.%s.port", Const.SSH_SERVER, server));
        final String user = properties.getProperty(String.format("%s.%s.user", Const.SSH_SERVER, server));
        final String pass = properties.getProperty(String.format("%s.%s.pass", Const.SSH_SERVER, server));
        logger.info(String.format("Authenticate: HOST=[%s] PORT=[%s]", host, port));
        doTestConnectivityServer(host, port, user, pass);
    }

    private void doTestConnectivityServer(final String host, final String port,
                                          final String user, final String pass) throws IOException {
        // connect
        final Connection connection = new Connection(host, Integer.parseInt(port));
        connection.connect();
        final String[] remainingAuthMethods = connection.getRemainingAuthMethods(user);
        for (String remainingAuthMethod : remainingAuthMethods) {
            logger.info(remainingAuthMethod);
        }
        // authenticate
        if (connection.isAuthMethodAvailable(user, "keyboard-interactive")) {
            final Properties properties = new Properties();
            properties.setProperty("//Password: ", pass);
            final InteractiveCallback callback = new InteractiveCallbackImpl(properties);
            final boolean authenticated = connection.authenticateWithKeyboardInteractive(user, callback);
            if (!authenticated) {
                throw new SecurityException(user);
            }
        }
        // authenticate
        if (connection.isAuthMethodAvailable(user, "password")) {
            final boolean authenticated = connection.authenticateWithPassword(user, pass);
            if (!authenticated) {
                throw new SecurityException(user);
            }
        }
        logger.info(String.format("Authenticated: HOST=[%s] PORT=[%s]", host, port));
        // test sftp subsystem
        checkSFTP("/", connection);
        // test command subsystem
        final AppPrincipal principal = new AppPrincipal("root", CollectionU.toCollection("*"));
        final UserExecutor executor = new UserExecutor(principal, new Date(), null);
        checkCommandBuiltIn(connection, executor);
        checkCommandAdHoc(connection, executor);
        checkCommandExplicit(connection, executor);
        // execute command
        connection.close();
    }

    private void checkSFTP(final String directory, final Connection connection) throws IOException {
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        final PrintStream ps = new PrintStream(os, true, UTF8Codec.Const.UTF8);
        final SFTPv3Client client = new SFTPv3Client(connection);
        final List<SFTPv3DirectoryEntry> list = client.ls(directory);
        for (SFTPv3DirectoryEntry entry : list) {
            ps.println(entry.filename);
        }
        logger.info(UTF8Codec.toString(os.toByteArray()));
    }

    private void checkCommandBuiltIn(final Connection connection, final UserExecutor executor) throws IOException {
        final SSHConnection sshConnection = new SSHConnection(connection, "xterm");
        final ExecutorService executorStream = executor.getExecutorStream();
        final SSHConnectionX sshConnectionX = new SSHConnectionX(sshConnection, executorStream);
        final String uid = sshConnectionX.toNameUID(0);
        final String gid = sshConnectionX.toNameGID(0);
        logger.info(String.format("uid(0) = %s", uid));
        logger.info(String.format("gid(0) = %s", gid));
        Assertions.assertEquals("root", uid);
        Assertions.assertEquals("root", gid);
    }

    private void checkCommandAdHoc(final Connection connection, final UserExecutor executor) throws IOException {
        final SSHConnection sshConnection = new SSHConnection(connection, "xterm");
        final ExecutorService executorStream = executor.getExecutorStream();
        final ResultsContext resultsContext = ResultsContext.createEmpty();
        final Command command = new ScriptX(sshConnection, executorStream, resultsContext).runCommand("ls /");
        Assertions.assertEquals(Integer.valueOf(0), command.getExitValue());
        logger.info(command.getStdout());
    }

    private void checkCommandExplicit(final Connection connection, final UserExecutor executor) throws IOException {
        final SSHConnection sshConnection = new SSHConnection(connection, "xterm");
        final ExecutorService executorStream = executor.getExecutorStream();
        final ResultsContext resultsContext = ResultsContext.createEmpty();
        final Script script = new Script(null, new Date(), null, "ls /");
        final ScriptContext context = new ScriptContext(executorStream, resultsContext, sshConnection);
        final ScriptRunnable runnable = new ScriptRunnable(script, context);
        runnable.run();
        final Command command = script.getCommands().iterator().next();
        Assertions.assertEquals(Integer.valueOf(0), command.getExitValue());
        logger.info(command.getStdout());
    }

    public static class InteractiveCallbackImpl implements InteractiveCallback {
        private final Properties properties;

        public InteractiveCallbackImpl(final Properties properties) {
            this.properties = properties;
        }

        public final String[] replyToChallenge(final String name, final String instruction, final int numPrompts,
                                               final String[] prompt, final boolean[] echo) throws Exception {
            final String[] replies = new String[numPrompts];
            for (int i = 0; (i < numPrompts); i++) {
                final String key = String.format("%s/%s/%s", name, instruction, prompt[i]);
                final String value = properties.getProperty(key);
                if (value == null) {
                    throw new GeneralSecurityException(key);
                } else {
                    replies[i] = value;
                }
            }
            return replies;
        }
    }

    private static class Const {
        private static final String SSH_SERVER = "ssh.server";
    }
}
