package io.github.greyp9.arwo.lib.jcifs.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import jcifs.smb.NtlmPasswordAuthentication;
import jcifs.smb.SmbException;
import jcifs.smb.SmbFile;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

public class CIFSTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        System.setProperty("java.protocol.handler.pkgs", "sun.net.www.protocol");
        jcifs.Config.registerSmbURLHandler();
    }

    @Test
    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assume.assumeTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assert.assertTrue(properties.size() > 0);
        String sshServerList = properties.getProperty(Const.CIFS_SERVER);
        Assert.assertNotNull(sshServerList);
        String[] servers = sshServerList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(String server, Properties properties) throws IOException {
        String url = properties.getProperty(String.format("%s.%s.url", Const.CIFS_SERVER, server));
        String user = properties.getProperty(String.format("%s.%s.user", Const.CIFS_SERVER, server));
        String pass = properties.getProperty(String.format("%s.%s.pass", Const.CIFS_SERVER, server));
        logger.info(String.format("Authenticate: URL=[%s]", url));
        doTestConnectivityServer(url, user, pass);
    }

    private void doTestConnectivityServer(String url, String user, String pass) throws IOException {
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(os, true, UTF8Codec.Const.UTF8);
        NtlmPasswordAuthentication auth = new NtlmPasswordAuthentication("", user, pass);
        SmbFile smbFolder = new SmbFile(url, auth);
        SmbFile[] smbFiles = smbFolder.listFiles();
        for (SmbFile smbFile : smbFiles) {
            ps.println(display(smbFile));
        }
        logger.info(UTF8Codec.toString(os.toByteArray()));
    }

    private static String display(SmbFile smbFile) throws SmbException {
        StringBuilder builder = new StringBuilder();
        builder.append(String.format("[name=%s]%n", smbFile.getName()));
        builder.append(new Date(smbFile.createTime()));
        builder.append("\n");
        builder.append(new Date(smbFile.lastModified()));
        builder.append("\n");
        builder.append(smbFile.length());
        builder.append("\n");
        builder.append(smbFile.getAttributes());
        builder.append("\n");
        builder.append(String.format("  [canRead=%s]%n", smbFile.canRead()));
        builder.append(String.format("  [canWrite=%s]%n", smbFile.canWrite()));
        return builder.toString();
    }

    private static class Const {
        private static final String CIFS_SERVER = "cifs.server";
    }
}
