package io.github.greyp9.arwo.app.cifs.action;

import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.cifs.data.CIFSDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.jcifs.fs.connection.CIFSConnection;
import jcifs.smb.SmbFile;

import java.io.IOException;

public class CIFSCreateFile {
    private final CIFSRequest request;
    private final CIFSConnection connection;
    private final Bundle bundle;
    private final Alerts alerts;

    public CIFSCreateFile(final CIFSRequest request, final CIFSConnection connection) {
        this.request = request;
        this.connection = connection;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final String charset = request.getUserState().getCharset();
        //String text = new XedActionFileNew(request.getLocale()).getFile();  // filter POST data through document
        final String filename = httpArguments.getValue("fileNew.fileNewType.filename");
        final String content = httpArguments.getValue("fileNew.fileNewType.file");  // just grab from POST data
        final byte[] bytes = UTF8Codec.toBytes(content, charset);
        final int length = ByteU.length(bytes);
        // put data to remote
        final FileX fileX = new FileX(Value.join("", request.getPath(), filename));

        final CIFSDataSource source = new CIFSDataSource(request, connection);
        final SmbFile lstat = source.lstat(fileX.getPath());
        if (lstat.exists()) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format(
                    "SFTPHandlerPostMultipart.file.exists", fileX.getPath(), request.getServer())));
        } else {
            final String hash = HexCodec.encode(HashU.md5(bytes));
            source.write(bytes, fileX.getPath());
            alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                    "SFTPHandlerPostMultipart.file.target", fileX.getPath(), length, hash)));
        }
    }
}
