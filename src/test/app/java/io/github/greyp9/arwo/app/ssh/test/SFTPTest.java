package io.github.greyp9.arwo.app.ssh.test;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.SFTPv3FileAttributes;
import io.github.greyp9.arwo.app.ssh.connection.SSHAuthenticatorClient;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.security.realm.AppPrincipal;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.lib.ganymed.ssh.client.ClientParams;
import io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable.ScriptX;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

public class SFTPTest {
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
        final Date date = new Date();
        // establish connection
        final Connection connection = new Connection(host, Integer.parseInt(port));
        connection.connect();
        final ClientParams clientParams = new ClientParams(null, user, pass, null);
        //new SSHAuthenticatorServer(bundle, alerts).authenticate(connectionInfo, serverParams);  // ignore pubkey
        new SSHAuthenticatorClient(new Bundle(), new Alerts()).authenticate(connection, clientParams);
        final SSHConnection sshConnection = new SSHConnection(connection, "xterm");
        // acquire user.home
        final AppPrincipal principal = new AppPrincipal("root", CollectionU.toCollection("*"));
        final UserExecutor executor = new UserExecutor(principal, new Date(), null);
        final ExecutorService executorStream = executor.getExecutorStream();
        final ResultsContext resultsContext = ResultsContext.createEmpty();
        final Command command = new ScriptX(sshConnection, executorStream, resultsContext).runCommand("pwd");
        Assertions.assertEquals(Integer.valueOf(0), command.getExitValue());
        final String userHome = command.getStdout().trim();
        logger.info(userHome);
        // craft dummy context
        final ServletHttpRequest httpRequest = new ServletHttpRequest(null, date, null, null, null, null);
        final SFTPRequest request = new SFTPRequest(httpRequest, null);
        final SFTPDataSource dataSource = new SFTPDataSource(request, sshConnection);
        // precondition, user.home exists
        final SFTPv3FileAttributes attributesUserHome = dataSource.exists(userHome);
        Assertions.assertNotNull(attributesUserHome);
        // ensure test container folder
        final String pathContainer = PathU.toDir(userHome, getClass().getSimpleName());
        SFTPv3FileAttributes attributesContainer = dataSource.exists(pathContainer);
        if (attributesContainer == null) {
            dataSource.createDirectory(pathContainer, attributesUserHome.permissions);
            attributesContainer = dataSource.exists(pathContainer);
        }
        Assertions.assertNotNull(attributesContainer);
        // create test folder
        final String filename = DateX.toFilename(date);
        final String pathFolder = PathU.toDir(pathContainer, filename);
        dataSource.createDirectory(pathFolder, attributesContainer.permissions);
        final SFTPv3FileAttributes attributesFolderPre = dataSource.exists(pathFolder);
        Assertions.assertNotNull(attributesFolderPre);
        // create test file
        dataSource.write(UTF8Codec.toBytes(filename), pathFolder, filename, null);
        final String pathFile = PathU.toPath(pathFolder, filename);
        final SFTPv3FileAttributes attributesFileYes = dataSource.exists(pathFile);
        Assertions.assertNotNull(attributesFileYes);
        // remove test file
        dataSource.delete(pathFile);
        final SFTPv3FileAttributes attributesFileNo = dataSource.exists(pathFile);
        Assertions.assertNull(attributesFileNo);
        // remove test folder
        dataSource.deleteDirectory(pathFolder);
        final SFTPv3FileAttributes attributesFolderPost = dataSource.exists(pathFolder);
        Assertions.assertNull(attributesFolderPost);
    }

    private static class Const {
        private static final String SSH_SERVER = "ssh.server";
    }
}
