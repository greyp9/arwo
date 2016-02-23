package io.github.greyp9.arwo.app.ssh.sftp.data;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.SCPClient;
import ch.ethz.ssh2.SCPInputStream;
import ch.ethz.ssh2.SCPOutputStream;
import ch.ethz.ssh2.SFTPException;
import ch.ethz.ssh2.SFTPv3Client;
import ch.ethz.ssh2.SFTPv3DirectoryEntry;
import ch.ethz.ssh2.SFTPv3FileAttributes;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.TreeMap;

@SuppressWarnings("PMD.TooManyMethods")
public class SFTPDataSource {
    private final SFTPRequest request;
    private final SSHConnection connection;

    public SFTPDataSource(final SFTPRequest request, final SSHConnection sshConnection) {
        this.request = request;
        this.connection = sshConnection;
    }

    public final SFTPv3FileAttributes exists(final String path) throws IOException {
        SFTPv3FileAttributes attributes = null;
        final SFTPv3Client client = new SFTPv3Client(connection.getConnection());
        try {
            final Date date = new Date();
            attributes = client.lstat(path);
            connection.update(date);
        } catch (SFTPException e) {
            if (e.getServerErrorCode() != Const.ERR_NO_SUCH_FILE) {
                new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
            }
        } finally {
            client.close();
        }
        return attributes;
    }

    public final SFTPv3FileAttributes lstat(final String path) throws IOException {
        SFTPv3FileAttributes attributes = null;
        final Connection connectionSSH = connection.getConnection();
        final SFTPv3Client client = new SFTPv3Client(connectionSSH);
        try {
            final Date date = new Date();
            attributes = client.lstat(path);
            connection.update(date);
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
        final SFTPv3Client client = new SFTPv3Client(connection.getConnection());
        try {
            final Date date = new Date();
            ls2(directoryEntries, client.ls(path));
            connection.update(date);
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

    public final void createDirectory(final String path, final int perms) throws IOException {
        final SFTPv3Client client = new SFTPv3Client(connection.getConnection());
        try {
            final Date date = new Date();
            client.mkdir(path, perms);
            connection.update(date);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        } finally {
            client.close();
        }
    }

    public final void deleteDirectory(final String path) throws IOException {
        final SFTPv3Client client = new SFTPv3Client(connection.getConnection());
        try {
            final Date date = new Date();
            client.rmdir(path);
            connection.update(date);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        } finally {
            client.close();
        }
    }

    public final void move(final String to, final String from) throws IOException {
        final SFTPv3Client client = new SFTPv3Client(connection.getConnection());
        try {
            final Date date = new Date();
            client.mv(from, to);
            connection.update(date);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        } finally {
            client.close();
        }
    }

    public final void delete(final String path) throws IOException {
        final SFTPv3Client client = new SFTPv3Client(connection.getConnection());
        try {
            final Date date = new Date();
            client.rm(path);
            connection.update(date);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        } finally {
            client.close();
        }
    }

    public final Collection<SFTPv3DirectoryEntry> lsSymlink(final String path) throws IOException {
        final Collection<SFTPv3DirectoryEntry> directoryEntries = new ArrayList<SFTPv3DirectoryEntry>();
        final SFTPv3Client client = new SFTPv3Client(connection.getConnection());
        try {
            final Date date = new Date();
            lsSymlink2(directoryEntries, path, client);
            connection.update(date);
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
        Integer type = App.FS.S_IFLNK;
        while (type == App.FS.S_IFLNK) {
            // assemble row entry
            final SFTPv3DirectoryEntry directoryEntry = new SFTPv3DirectoryEntry();
            directoryEntry.filename = pathIt;
            directoryEntry.attributes = client.lstat(pathIt);
            directoryEntries.add(directoryEntry);
            type = SFTPFolder.toType(directoryEntry.attributes);
            if (type == App.FS.S_IFLNK) {
                pathIt = client.readLink(pathIt);
            }
        }
    }

    @SuppressWarnings({ "PMD.UseConcurrentHashMap", "PMD.AvoidInstantiatingObjectsInLoops" })
    public final Map<String, Collection<SFTPv3DirectoryEntry>> find(final String path) throws IOException {
        final Map<String, Collection<SFTPv3DirectoryEntry>> find =
                new TreeMap<String, Collection<SFTPv3DirectoryEntry>>();
        final SFTPv3Client client = new SFTPv3Client(connection.getConnection());
        final Stack<String> paths = new Stack<String>();
        paths.push(path);
        while (!paths.isEmpty()) {
            final String pathIt = paths.pop();
            final Collection<SFTPv3DirectoryEntry> directoryEntries = new ArrayList<SFTPv3DirectoryEntry>();
            final Date date = new Date();
            ls2(directoryEntries, client.ls(pathIt));
            connection.update(date);
            find.put(pathIt, directoryEntries);
            for (final SFTPv3DirectoryEntry directoryEntry : directoryEntries) {
                if (shouldRecurse(directoryEntry)) {
                    paths.push(pathIt + directoryEntry.filename + Http.Token.SLASH);
                }
            }
        }
        return find;
    }

    private boolean shouldRecurse(final SFTPv3DirectoryEntry directoryEntry) {
        boolean shouldRecurse = false;
        if (directoryEntry.attributes.isDirectory()) {
            if (".".equals(directoryEntry.filename)) {
                directoryEntry.getClass();
            } else if ("..".equals(directoryEntry.filename)) {
                directoryEntry.getClass();
            } else {
                shouldRecurse = true;
            }
        }
        return shouldRecurse;
    }

    public final NameTypeValues properties(final String path) throws IOException {
        final NameTypeValues nameTypeValues = new NameTypeValues();
        final SFTPv3FileAttributes attributes = lstat(path);
        nameTypeValues.add("sftpPropertiesType.attributes", attributes.getOctalPermissions());
        nameTypeValues.add("sftpPropertiesType.length", Long.toString(attributes.size));
        nameTypeValues.add("sftpPropertiesType.mtime", DateU.fromSeconds(attributes.mtime));
        nameTypeValues.add("sftpPropertiesType.atime", DateU.fromSeconds(attributes.atime));
        return nameTypeValues;
    }

    @SuppressWarnings("PMD.CloseResource")
    public final MetaFile read(final String path) throws IOException {
        long lastModified = 0L;
        byte[] bytes = new byte[0];
        try {
            final SCPClient client = connection.getConnection().createSCPClient();
            lastModified = DateU.fromSeconds(lstat(path).mtime).getTime();
            final Date date = new Date();
            bytes = read(client.get(path));
            connection.update(date);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        }
        final FileMetaData metaData = new FileMetaData(path, bytes.length, lastModified, false);
        return new MetaFile(metaData, null, new ByteArrayInputStream(bytes));
    }

    private static byte[] read(final SCPInputStream is) throws IOException {
        try {
            return StreamU.read(is);
        } finally {
            is.close();
        }
    }

    @SuppressWarnings("PMD.CloseResource")
    public final void write(
            final byte[] bytes, final String folder, final String filename, final String mode) throws IOException {
        try {
            final SCPClient client = connection.getConnection().createSCPClient();
            final Date date = new Date();
            write(bytes, client.put(filename, bytes.length, folder, mode));
            connection.update(date);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        }
    }

    private static void write(final byte[] bytes, final SCPOutputStream os) throws IOException {
        StreamU.write(os, bytes);
    }

/* was needed for lib versions prior to "ganymed-ssh2-262.jar"
    private static String normalize(final String path) {
        return path.replace(" ", "\\ ").replace("(", "\\(").replace(")", "\\)");  // lib quirk  // i18n comment
    }
*/

    private static class Const {
        private static final int ERR_NO_SUCH_FILE = 2;
    }
}
