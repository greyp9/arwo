package io.github.greyp9.arwo.lib.sardine.test;

import com.github.sardine.DavResource;
import com.github.sardine.Sardine;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.SardineFactory;
import junit.framework.TestCase;
import org.junit.Assert;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SardineTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testServerConnectivity() throws Exception {
        File fileProperties = new File(SystemU.userHome(), ".arwo/test.properties.xml");
        Assert.assertTrue(fileProperties.exists());
        Properties properties = PropertiesU.loadFromXml(fileProperties.toURI().toURL());
        logger.info("" + properties.size());
        Assert.assertTrue(properties.size() > 0);
        String serverList = properties.getProperty(Const.WEBDAV_SERVER);
        Assert.assertNotNull(serverList);
        String[] servers = serverList.split(",");
        for (String server : servers) {
            if (server.length() > 0) {
                doTestConnectivityServer(server, properties);
            }
        }
    }

    private void doTestConnectivityServer(String server, Properties properties) throws IOException {
        String certificate = properties.getProperty(String.format("%s.%s.certificate", Const.WEBDAV_SERVER, server));
        String protocol = properties.getProperty(String.format("%s.%s.protocol", Const.WEBDAV_SERVER, server));
        String host = properties.getProperty(String.format("%s.%s.host", Const.WEBDAV_SERVER, server));
        String port = properties.getProperty(String.format("%s.%s.port", Const.WEBDAV_SERVER, server));
        String user = properties.getProperty(String.format("%s.%s.user", Const.WEBDAV_SERVER, server));
        String pass = properties.getProperty(String.format("%s.%s.pass", Const.WEBDAV_SERVER, server));
        logger.info(String.format("Authenticate: HOST=[%s] PORT=[%s]", host, port));
        doTestConnectivityServer(certificate, protocol, host, port, user, pass);
    }

    private void doTestConnectivityServer(String certificate, String protocol,
                                          String host, String port, String user, String pass) throws IOException {
        final Sardine connection = new SardineFactory().createConnection(certificate);
        connection.setCredentials(user, pass);
        final URL url = new URL(protocol, host, NumberU.toInt(port, 0), Http.Token.SLASH);
        enumerateRecurse(url, connection);
    }

    private void enumerateRecurse(URL url, Sardine connection) throws IOException {
        logger.info(String.format("QUERY:[%s]", url.toExternalForm()));
        List<DavResource> resources = connection.list(url.toExternalForm());
        for (DavResource resource : resources) {
            logger.info(String.format("[%s]", resource.toString()));
        }
        for (DavResource resource : resources) {
            iterate(url, resource, connection);
        }
    }

    private void iterate(URL url, DavResource resource, Sardine connection) throws IOException {
        // filter out identity resource
        final String pathURL = URLCodec.decode(url.getFile());
        final String pathResource = resource.getPath();
        if (pathURL.equals(pathResource)) {
            return;
        }
        String pathEncoded = url.getFile() + URLCodec.encode(new FileX(pathResource).getFilename()) +
                (pathResource.endsWith("/") ? "/" : "");
        URL urlChild = new URL(url.getProtocol(), url.getHost(), url.getPort(), pathEncoded);
        final boolean isFolderURL = pathURL.endsWith("/");
        final boolean isFolderResource = pathResource.endsWith("/");
        if (isFolderURL || isFolderResource) {
            enumerateRecurse(urlChild, connection);
        } else {
            String pathEncoded2 = url.getFile() + "/" + URLCodec.encode(new FileX(pathResource).getFilename()) +
                    (pathResource.endsWith("/") ? "/" : "");
            URL urlChild2 = new URL(url.getProtocol(), url.getHost(), url.getPort(), pathEncoded2);
            read(urlChild2, resource, connection);
        }
    }

    private void read(URL url, DavResource resource, Sardine connection) throws IOException {
        //logger.info(String.format("READ:[%s]", resource.toString()));
        logger.info(String.format("READ:[%s]", url.toExternalForm()));
        try {
            final byte[] bytes = StreamU.read(connection.get(url.toExternalForm()));
            final String hash = HexCodec.encode(HashU.md5(bytes));
            final Date modified = resource.getModified();
            FileMetaData metaData = new FileMetaData(
                    resource.getPath(), bytes.length, ((modified == null) ? 0L : modified.getTime()), false);
            //MetaFile metaFile = new MetaFile(metaData, new ByteArrayInputStream(bytes));
            logger.info(String.format(Const.PATTERN_ENTRY,
                    metaData.getLength(), hash, metaData.getLastModified(), metaData.getPath()));
        } catch (IOException e) {
            logger.log(Level.WARNING, e.getClass().getName(), e);
        }
    }

    private static class Const {
        private static final String WEBDAV_SERVER = "webdav.server";
        private static final String PATTERN_ENTRY = "%10d  %s  %s  %s";
    }
}
