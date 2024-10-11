package io.github.greyp9.arwo.lib.jinterop.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

public class InteropTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assumptions.assumeTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assertions.assertTrue(properties.size() > 0);
        String sshServerList = properties.getProperty(Const.INTEROP_SERVER);
        Assertions.assertNotNull(sshServerList);
        String[] servers = sshServerList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(final String server, final Properties properties) throws IOException {
        String host = properties.getProperty(String.format("%s.%s.host", Const.INTEROP_SERVER, server));
        String user = properties.getProperty(String.format("%s.%s.user", Const.INTEROP_SERVER, server));
        String pass = properties.getProperty(String.format("%s.%s.pass", Const.INTEROP_SERVER, server));
        logger.info(String.format("Authenticate: HOST=[%s]", host));
        doTestConnectivityServer(host, user, pass);
    }

    private void doTestConnectivityServer(final String host, final String user, final String pass) throws IOException {
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(os, true, UTF8Codec.Const.UTF8);
        InteropShell shell = new InteropShell(host, user, pass);
        Collection<String> stdins = Arrays.asList("dir C:\\", "dir D:\\", "dir E:\\");
        Collection<Command> commands = shell.runCommands(stdins);
        for (Command command : commands) {
            ps.println("---------=---------=---------=---------=---------=---------=");
            ps.println(String.format("> %s", command.getStdin()));
            ps.println(String.format("[%s] [%d] [%d]",
                    command.getStart(), command.getElapsed(new Date()), command.getExitValue()));
            ps.println(String.format("<STDERR>%s</STDERR>", command.getStderr()));
            ps.println(command.getStdout());
            ps.println("---------=---------=---------=---------=---------=---------=");
        }
        logger.info(UTF8Codec.toString(os.toByteArray()));
    }

    private static class Const {
        private static final String INTEROP_SERVER = "interop.server";
    }
}
