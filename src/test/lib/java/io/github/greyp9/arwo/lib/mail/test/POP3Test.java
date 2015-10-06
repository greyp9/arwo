package io.github.greyp9.arwo.lib.mail.test;

import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import junit.framework.TestCase;
import org.junit.Assert;

import javax.mail.*;
import java.io.File;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Logger;

public class POP3Test extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assert.assertTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assert.assertTrue(properties.size() > 0);
        String serverList = properties.getProperty(Const.POP3_SERVER);
        Assert.assertNotNull(serverList);
        String[] servers = serverList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(String server, Properties properties) throws IOException, MessagingException {
        String protocol = properties.getProperty(String.format("%s.%s.protocol", Const.POP3_SERVER, server));
        String host = properties.getProperty(String.format("%s.%s.host", Const.POP3_SERVER, server));
        String port = properties.getProperty(String.format("%s.%s.port", Const.POP3_SERVER, server));
        String user = properties.getProperty(String.format("%s.%s.user", Const.POP3_SERVER, server));
        String pass = properties.getProperty(String.format("%s.%s.pass", Const.POP3_SERVER, server));
        logger.info(String.format("Authenticate: HOST=[%s] PORT=[%s]", host, port));
        doTestConnectivityServer(protocol, host, port, user, pass);
    }

    private void doTestConnectivityServer(String protocol, String host, String port,
                                          String user, String pass) throws IOException, MessagingException {
        Properties properties = new Properties();
        //properties.setProperty("mail.debug", "true");
        properties.setProperty("mail.store.protocol", protocol);
        Session session = Session.getInstance(properties);
        Store store = session.getStore(protocol);
        store.connect(host, Integer.parseInt(port), user, pass);
        Folder[] folders = store.getDefaultFolder().list();
        for (Folder folder : folders) {
            logger.info(String.format("[%s][%s]", folder.getName(), folder.getFullName()));
            if ("INBOX".equals(folder.getName())) {
                folder.open(Folder.READ_ONLY);
                int lastMessage = folder.getMessageCount();
                int firstMessage = (lastMessage - 2);
                logger.info(String.format("GET FROM [%d] TO [%d]", firstMessage, lastMessage));
                Message[] messages = folder.getMessages(firstMessage, lastMessage);
                for (Message message : messages) {
                    logger.info(String.format("[%d][%s]", message.getMessageNumber(), message.getSubject()));
                }
                folder.close(false);
            }
        }
        store.close();
    }

    private static class Const {
        private static final String POP3_SERVER = "pop3.server";
    }
}
