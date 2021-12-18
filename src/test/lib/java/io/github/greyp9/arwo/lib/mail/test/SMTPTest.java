package io.github.greyp9.arwo.lib.mail.test;

import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Test;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.io.File;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

public class SMTPTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assume.assumeTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assert.assertTrue(properties.size() > 0);
        String serverList = properties.getProperty(Const.SMTP_SERVER);
        Assert.assertNotNull(serverList);
        String[] servers = serverList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(String server, Properties properties) throws MessagingException {
        String protocol = properties.getProperty(String.format("%s.%s.protocol", Const.SMTP_SERVER, server));
        String host = properties.getProperty(String.format("%s.%s.host", Const.SMTP_SERVER, server));
        String port = properties.getProperty(String.format("%s.%s.port", Const.SMTP_SERVER, server));
        String user = properties.getProperty(String.format("%s.%s.user", Const.SMTP_SERVER, server));
        String pass = properties.getProperty(String.format("%s.%s.pass", Const.SMTP_SERVER, server));
        //String certificate = properties.getProperty(String.format("%s.%s.cert", Const.SMTP_SERVER, server));
        String to = properties.getProperty(String.format("%s.%s.to", Const.SMTP_SERVER, server));
        String subject = properties.getProperty(String.format("%s.%s.subject", Const.SMTP_SERVER, server));
        logger.info(String.format("Authenticate: HOST=[%s] PORT=[%s]", host, port));
        doTestConnectivityServer(protocol, host, port, user, pass, to, subject);
    }

    private void doTestConnectivityServer(String protocol, String host, String port, String user, String pass,
                                          String to, String subject) throws MessagingException {
        Properties properties = new Properties();
        //properties.setProperty("mail.debug", "true");
        Session session = Session.getInstance(properties);
        sendMessage(protocol, host, Integer.parseInt(port), user, pass, user, to, subject, session);
    }

    private void sendMessage(String protocol, String host, int port, String user, String password,
                             String from, String to, String subject, Session session) throws MessagingException {
        Date date = new Date();
        Transport transport = session.getTransport(protocol);
        transport.connect(host, port, user, password);
        MimeMessage mimeMessage = new MimeMessage(session);
        mimeMessage.setFrom(new InternetAddress(from));
        mimeMessage.setRecipient(Message.RecipientType.TO, new InternetAddress(to));
        mimeMessage.setSentDate(date);
        mimeMessage.setSubject(subject);
        mimeMessage.setText(date.toString());
        mimeMessage.saveChanges();
        transport.sendMessage(mimeMessage, mimeMessage.getAllRecipients());
        transport.close();
        logger.info("Message sent.");
    }

    private static class Const {
        private static final String SMTP_SERVER = "smtp.server";
    }
}
