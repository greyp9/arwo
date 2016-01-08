package io.github.greyp9.arwo.app.cifs.data;

import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.lib.jcifs.fs.connection.CIFSConnection;
import jcifs.smb.ACE;
import jcifs.smb.SmbFile;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

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
            final SmbFile smbFile = connection.getSmbFile(path);
            Collections.addAll(directoryEntries, smbFile.listFiles());
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        return directoryEntries;
    }

    public NameTypeValues properties(final String path) throws IOException {
        NameTypeValues nameValues = new NameTypeValues();
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
            ACE[] security = smbFile.getSecurity();
            for (ACE ace : security) {
                String sid = ace.getSID().toDisplayString();
                String allow = (ace.isAllow() ? "ALLOW" : "DENY");
                String access = (Integer.toHexString(ace.getAccessMask()));
                //String accessDisplay = CIFSDataSourceU.toDisplayAccess(ace.getAccessMask());
                String flags = (Integer.toHexString(ace.getFlags()));
                String value = String.format("[%s] ACCESS:[%s] FLAGS:[%s]", allow, access, flags);
                nameValues.add(new NameTypeValue("[ACE]" + sid, null, value));
                //nameValues.add(new NameValue(String.format("[%s]", sid), ace.toString()));
            }
        } catch (IOException e) {
            new ExceptionModel(alerts).service(e, Alert.Severity.ERR);
        }
        return nameValues;
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
