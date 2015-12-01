package io.github.greyp9.arwo.app.ssh.sftp.view;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.SFTPv3Client;
import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnection;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;

public class SFTPResourceView {
    private final SFTPRequest request;
    private final AppUserState userState;
    private final SSHConnectionResource resource;

    public SFTPResourceView(
            final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        this.request = request;
        this.userState = userState;
        this.resource = resource;
    }

    public final HttpResponse doGetResource() throws IOException {
        request.getClass();
        userState.getClass();
        final SSHConnection sshConnection = resource.getSSHConnection();
        final String text = UTF8Codec.toString(list("/", sshConnection.getConnection()));
        return HttpResponseU.to200Text(text);
    }

    private byte[] list(final String directory, final Connection connection) throws IOException {
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        final PrintStream ps = new PrintStream(os, true, UTF8Codec.Const.UTF8);
        final SFTPv3Client client = new SFTPv3Client(connection);
        final List<SFTPv3DirectoryEntry> list = client.ls(directory);
        for (final SFTPv3DirectoryEntry entry : list) {
            ps.println(entry.filename);
        }
        return os.toByteArray();
    }
}
