package io.github.greyp9.arwo.lib.jinterop.test;

import io.github.greyp9.arwo.core.command.Command;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import junit.framework.TestCase;
import org.junit.Assert;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class InteropTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        Logger.getLogger("org.jinterop").setLevel(Level.SEVERE);
    }

    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assert.assertTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assert.assertTrue(properties.size() > 0);
        String sshServerList = properties.getProperty(Const.INTEROP_SERVER);
        Assert.assertNotNull(sshServerList);
        String[] servers = sshServerList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(String server, Properties properties) throws IOException {
        String host = properties.getProperty(String.format("%s.%s.host", Const.INTEROP_SERVER, server));
        String user = properties.getProperty(String.format("%s.%s.user", Const.INTEROP_SERVER, server));
        String pass = properties.getProperty(String.format("%s.%s.pass", Const.INTEROP_SERVER, server));
        logger.info(String.format("Authenticate: HOST=[%s]", host));
        doTestConnectivityServer(host, user, pass);
    }

    private void doTestConnectivityServer(String host, String user, String pass) throws IOException {
        InteropShell shell = new InteropShell(host, user, pass);
        Collection<String> stdins = Arrays.asList("dir C:\\", "dir D:\\", "dir E:\\");
        Collection<Command> commands = shell.runCommands(stdins);
        for (Command command : commands) {
            logger.info("---------=---------=---------=---------=---------=---------=");
            logger.info(String.format("> %s", command.getStdin()));
            logger.info(String.format("[%s] [%d] [%d]",
                    command.getDateStart(), command.getElapsed(), command.getExitValue()));
            logger.info(String.format("<STDERR>%s</STDERR>", command.getStderr()));
            logger.info(command.getStdout());
            logger.info("---------=---------=---------=---------=---------=---------=");
        }
    }

    private static class Const {
        private static final String INTEROP_SERVER = "interop.server";
    }
}
