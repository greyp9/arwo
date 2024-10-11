package io.github.greyp9.arwo.app.ssh.sftp.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionFactory;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.http.form.MimeHeader;
import io.github.greyp9.arwo.core.http.form.MimePart;
import io.github.greyp9.arwo.core.http.form.MultipartForm;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Properties;

public class SFTPHandlerPostMultipart {
    private final SFTPRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Alerts alerts;

    public SFTPHandlerPostMultipart(final SFTPRequest request, final AppUserState userState) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final String doPostMultipartFormData(final String location) throws IOException {
        final ByteArrayInputStream is = httpRequest.getHttpRequest().getEntity();
        final MultipartForm form = new MultipartForm(is);
        for (final Iterator<MimePart> mimePartIt = form.iterator(); mimePartIt.hasNext(); mimePartIt.getClass()) {
            final MimePart mimePart = mimePartIt.next();
            final Properties propertiesPart = new Properties();
            for (final Iterator<MimeHeader> headerIt = mimePart.iterator(); headerIt.hasNext(); headerIt.getClass()) {
                final MimeHeader mimeHeader = headerIt.next();
                mimeHeader.addTo(propertiesPart);
            }
            final String name = propertiesPart.getProperty(App.Post.CD_NAME);
            if (App.Post.UPLOAD_FILE.equals(name)) {
                doPostUploadFile(mimePart, propertiesPart);
            }
        }
        return location;
    }

    private void doPostUploadFile(final MimePart mimePart, final Properties properties) throws IOException {
        final String server = request.getServer();
        final String filename = properties.getProperty(App.Post.CD_FILENAME);
        final SSHConnectionFactory factory = new SSHConnectionFactory(httpRequest, userState, bundle, alerts);
        final SSHConnectionResource resource = (SSHConnectionResource)
                userState.getSSH().getCache().getResource(server, factory);
        if (resource == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SFTPHandlerPostMultipart.no.connect", server)));
        } else if (filename.length() == 0) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SFTPHandlerPostMultipart.no.file", server)));
        } else {
            doPostUploadFile(mimePart, filename, resource);
            resource.getConnection().update(httpRequest.getDate());
        }
    }

    private void doPostUploadFile(
            final MimePart mimePart, final String filename, final SSHConnectionResource resource) throws IOException {
        final byte[] bytes = mimePart.getBody().toByteArray();
        alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                "SFTPHandlerPostMultipart.file.source", filename)));
        // put data to remote
        final FileX fileX = new FileX(Value.join("", request.getPath(), filename));
        final SFTPDataSource source = new SFTPDataSource(request, resource.getConnection());
        source.write(bytes, fileX.getFolder(), fileX.getFilename(), null);
        // info alert
        final String hash = HexCodec.encode(HashU.md5(bytes));
        alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                "SFTPHandlerPostMultipart.file.target", fileX.getFolderSlash(), bytes.length, hash)));
    }
}
