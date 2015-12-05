package io.github.greyp9.arwo.app.ssh.sftp.action;

import io.github.greyp9.arwo.app.ssh.connection.SSHConnection;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

public class SFTPUpdateFile {
    private final SFTPRequest request;
    private final SSHConnection sshConnection;

    public SFTPUpdateFile(final SFTPRequest request, final SSHConnection sshConnection) {
        this.request = request;
        this.sshConnection = sshConnection;
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final Collection<String> utf16Modes = Arrays.asList("view16", "edit16");
        final boolean isUTF16 = utf16Modes.contains(request.getMode());
        final String encoding = (isUTF16 ? UTF8Codec.Const.UTF16 : UTF8Codec.Const.UTF8);
        //String text = new XedActionFile(request.getLocale()).getFile();  // filter POST data through document
        final String text = httpArguments.getValue("file.fileType.file");  // more efficient to just grab from POST data
        final byte[] bytes = UTF8Codec.toBytes(text, encoding);
        // put data to remote
        final FileX file = new FileX(request.getPath());
        final SFTPDataSource source = new SFTPDataSource(request, sshConnection);
        source.write(bytes, file.getFolder(), file.getFilename());
        // info alert
        final String hash = HexCodec.encode(HashU.md5(bytes));
        request.getAlerts().add(new Alert(Alert.Severity.INFO, request.getBundle().format(
                "SFTPHandlerPostMultipart.file.target", file.getFolderSlash(), bytes.length, hash)));
    }
}
