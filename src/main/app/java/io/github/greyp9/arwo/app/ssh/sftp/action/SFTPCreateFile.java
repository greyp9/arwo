package io.github.greyp9.arwo.app.ssh.sftp.action;

import ch.ethz.ssh2.SFTPv3FileAttributes;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
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
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

public class SFTPCreateFile {
    private final SFTPRequest request;
    private final SSHConnection sshConnection;
    private final Bundle bundle;
    private final Alerts alerts;

    public SFTPCreateFile(final SFTPRequest request, final SSHConnection sshConnection) {
        this.request = request;
        this.sshConnection = sshConnection;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final Collection<String> utf16Modes = Arrays.asList("view16", "edit16", "create16");
        final boolean isUTF16 = utf16Modes.contains(request.getMode());
        final String encoding = (isUTF16 ? UTF8Codec.Const.UTF16 : UTF8Codec.Const.UTF8);
        //String text = new XedActionFileNew(request.getLocale()).getFile();  // filter POST data through document
        final String filename = httpArguments.getValue("fileNew.fileNewType.filename");
        final String content = httpArguments.getValue("fileNew.fileNewType.file");  // just grab from POST data
        final byte[] bytes = UTF8Codec.toBytes(content, encoding);
        final int length = ByteU.length(bytes);
        // put data to remote
        final FileX file = new FileX(Value.join("", request.getPath(), filename));
        final SFTPDataSource source = new SFTPDataSource(request, sshConnection);
        final SFTPv3FileAttributes lstat = source.exists(file.getPath());
        if (lstat == null) {
            final String hash = HexCodec.encode(HashU.md5(bytes));
            source.write(bytes, file.getFolder(), file.getFilename());
            alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                    "SFTPHandlerPostMultipart.file.target", file.getPath(), length, hash)));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format(
                    "SFTPHandlerPostMultipart.file.exists", file.getPath(), request.getServer())));
        }
    }
}
