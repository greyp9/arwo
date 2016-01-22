package io.github.greyp9.arwo.app.cifs.data;

import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.lib.jcifs.fs.connection.CIFSConnection;
import jcifs.smb.ACE;
import jcifs.smb.SmbFile;
import jcifs.smb.SmbFileOutputStream;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

public class CIFSDataSource {
    private final CIFSRequest request;
    private final CIFSConnection connection;
    private final Alerts alerts;

    public CIFSDataSource(final CIFSRequest request, final CIFSConnection connection) {
        this.request = request;
        this.connection = connection;
        this.alerts = request.getAlerts();
    }

    public final SmbFile lstat(final String path) throws IOException {
        SmbFile smbFile = null;
        final Date date = new Date();
        try {
            smbFile = connection.getSmbFile(path);
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        connection.update(date);
        return smbFile;
    }

    public final Collection<SmbFile> listFiles(final String path) throws IOException {
        final Collection<SmbFile> directoryEntries = new ArrayList<SmbFile>();
        final Date date = new Date();
        try {
            final SmbFile smbFile = connection.getSmbFile(path);
            Collections.addAll(directoryEntries, smbFile.listFiles());
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        connection.update(date);
        return directoryEntries;
    }

    public final void createDirectory(final String path) throws IOException {
        final Date date = new Date();
        try {
            final SmbFile smbFile = connection.getSmbFile(path);
            smbFile.mkdir();
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        connection.update(date);
    }

    public final void deleteDirectory(final String path) throws IOException {
        final Date date = new Date();
        try {
            final SmbFile smbFile = connection.getSmbFile(path);
            smbFile.delete();
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        connection.update(date);
    }

    public final void move(final String to, final String from, final boolean overwrite) throws IOException {
        final String message = request.getBundle().format("alert.action.not.implemented", to, from, overwrite);
        alerts.add(new Alert(Alert.Severity.WARN, message));
    }

    public final NameTypeValues properties(final String path) throws IOException {
        final NameTypeValues nameValues = new NameTypeValues();
        final Date date = new Date();
        try {
            final SmbFile smbFile = connection.getSmbFile(path);
            nameValues.add(new NameTypeValue("attributes", null, Integer.toHexString(smbFile.getAttributes())));
            nameValues.add(new NameTypeValue("length", null, Long.toString(smbFile.length())));
            nameValues.add(new NameTypeValue("available", null, Long.toString(smbFile.getDiskFreeSpace())));
            nameValues.add(new NameTypeValue("principal", null, smbFile.getPrincipal().toString()));
            nameValues.add(new NameTypeValue("createTime", null, new Date(smbFile.createTime())));
            nameValues.add(new NameTypeValue("updateTime", null, new Date(smbFile.lastModified())));
            nameValues.add(new NameTypeValue("R", null, Boolean.toString(smbFile.canRead())));
            nameValues.add(new NameTypeValue("W", null, Boolean.toString(smbFile.canWrite())));
            final ACE[] security = smbFile.getSecurity();
            for (final ACE ace : security) {
                final String sid = ace.getSID().toDisplayString();
                final String allow = (ace.isAllow() ? "ALLOW" : "DENY");
                final String access = Integer.toHexString(ace.getAccessMask());
                //String accessDisplay = CIFSDataSourceU.toDisplayAccess(ace.getAccessMask());
                final String flags = Integer.toHexString(ace.getFlags());
                final String value = String.format("[%s] ACCESS:[%s] FLAGS:[%s]", allow, access, flags);
                nameValues.add(NameTypeValue.U.create("[ACE]" + sid, value));
                //nameValues.add(new NameValue(String.format("[%s]", sid), ace.toString()));
            }
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        connection.update(date);
        return nameValues;
    }

    public final void delete(final String path) {
        final Date date = new Date();
        try {
            final SmbFile smbFile = connection.getSmbFile(path);
            smbFile.delete();
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        connection.update(date);
    }

    public final MetaFile read(final String path) throws IOException {
        long lastModified = 0L;
        byte[] bytes = new byte[0];
        final Date date = new Date();
        try {
            final SmbFile smbFile = lstat(path);
            lastModified = smbFile.getLastModified();
            bytes = StreamU.read(smbFile.getInputStream());
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        connection.update(date);
        final FileMetaData metaData = new FileMetaData(path, bytes.length, lastModified, false);
        return new MetaFile(metaData, new ByteArrayInputStream(bytes));
    }

    public final void write(final byte[] bytes, final String path) throws IOException {
        final Date date = new Date();
        try {
            final SmbFile smbFile = connection.getSmbFile(path);
            final SmbFileOutputStream os = new SmbFileOutputStream(smbFile);
            StreamU.write(os, bytes);
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        connection.update(date);
    }
}
