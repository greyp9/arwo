package io.github.greyp9.arwo.lib.ganymed.test;

import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.Properties;
import java.util.logging.Logger;

public class SFTPTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assumptions.assumeTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assertions.assertTrue(properties.size() > 0);
        String sshServerList = properties.getProperty(Const.SSH_SERVER);
        Assertions.assertNotNull(sshServerList);
        String[] servers = sshServerList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                logger.finest(server);
            }
        }
    }

    private static class Const {
        private static final String SSH_SERVER = "ssh.server";
    }
}
