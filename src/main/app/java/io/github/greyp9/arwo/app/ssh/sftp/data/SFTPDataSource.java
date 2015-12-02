package io.github.greyp9.arwo.app.ssh.sftp.data;

import ch.ethz.ssh2.SFTPv3Client;
import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import ch.ethz.ssh2.SFTPv3FileAttributes;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.lib.ganymed.ssh.core.SFTP;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class SFTPDataSource {
    private final SFTPRequest request;
    private final SSHConnectionResource resource;

    public SFTPDataSource(final SFTPRequest request, final SSHConnectionResource resource) {
        this.request = request;
        this.resource = resource;
    }

    public final SFTPv3FileAttributes lstat(final String path) throws IOException {
        SFTPv3FileAttributes attributes = null;
        final SFTPv3Client client = new SFTPv3Client(resource.getSSHConnection().getConnection());
        try {
            attributes = client.lstat(path);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        } finally {
            client.close();
        }
        return attributes;
    }

    @SuppressWarnings("PMD.ShortMethodName")
    public final Collection<SFTPv3DirectoryEntry> ls(final String path) throws IOException {
        final Collection<SFTPv3DirectoryEntry> directoryEntries = new ArrayList<SFTPv3DirectoryEntry>();
        final SFTPv3Client client = new SFTPv3Client(resource.getSSHConnection().getConnection());
        try {
            ls2(directoryEntries, client.ls(path));
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        } finally {
            client.close();
        }
        return directoryEntries;
    }

    private void ls2(final Collection<SFTPv3DirectoryEntry> directoryEntries, final List<SFTPv3DirectoryEntry> ls)
            throws IOException {
        for (final Object o : ls) {
            directoryEntries.add((SFTPv3DirectoryEntry) o);
        }
    }

    public final Collection<SFTPv3DirectoryEntry> lsSymlink(final String path) throws IOException {
        final Collection<SFTPv3DirectoryEntry> directoryEntries = new ArrayList<SFTPv3DirectoryEntry>();
        final SFTPv3Client client = new SFTPv3Client(resource.getSSHConnection().getConnection());
        try {
            lsSymlink2(directoryEntries, path, client);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        } finally {
            client.close();
        }
        return directoryEntries;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void lsSymlink2(final Collection<SFTPv3DirectoryEntry> directoryEntries,
                            final String path, final SFTPv3Client client) throws IOException {
        String pathIt = path;
        Integer type = SFTP.S_IFLNK;
        while (type == SFTP.S_IFLNK) {
            // assemble row entry
            final SFTPv3DirectoryEntry directoryEntry = new SFTPv3DirectoryEntry();
            directoryEntry.filename = pathIt;
            directoryEntry.attributes = client.lstat(pathIt);
            directoryEntries.add(directoryEntry);
            type = SFTPFolder.toType(directoryEntry.attributes);
            if (type == SFTP.S_IFLNK) {
                pathIt = client.readLink(pathIt);
            }
        }
    }
}
