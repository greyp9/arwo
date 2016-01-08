package io.github.greyp9.arwo.app.cifs.data;

import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.lib.jcifs.fs.connection.CIFSConnection;
import jcifs.smb.SmbFile;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class CIFSDataSource {
    //private final CIFSRequest request;
    private final CIFSConnection connection;
    private final Alerts alerts;

    public CIFSDataSource(final CIFSRequest request, final CIFSConnection connection) {
        //this.request = request;
        this.connection = connection;
        this.alerts = request.getAlerts();
    }

    public final SmbFile lstat(final String path) throws IOException {
        SmbFile smbFile = null;
        try {
            smbFile = connection.getSmbFile(path);
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        return smbFile;
    }

    public final Collection<SmbFile> listFiles(final String path) throws IOException {
        final Collection<SmbFile> directoryEntries = new ArrayList<SmbFile>();
        try {
            Collections.addAll(directoryEntries, connection.getSmbFile(path).listFiles());
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        return directoryEntries;
    }

    public final void delete(final String path) {
        path.getClass();
    }

    public final MetaFile read(final String path) throws IOException {
        long lastModified = 0L;
        byte[] bytes = new byte[0];
        try {
            final SmbFile smbFile = lstat(path);
            lastModified = smbFile.getLastModified();
            bytes = StreamU.read(smbFile.getInputStream());
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        final FileMetaData metaData = new FileMetaData(path, bytes.length, lastModified, false);
        return new MetaFile(metaData, new ByteArrayInputStream(bytes));
    }

    public final void write(final byte[] bytes, final String path) throws IOException {
        bytes.getClass();
        path.getClass();
    }
}
