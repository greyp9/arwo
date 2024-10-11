package io.github.greyp9.arwo.app.webdav.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.app.webdav.fs.data.WebDAVDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.WebDAVConnection;

import java.io.IOException;

public class WebDAVUpdateFile {
    private final WebDAVRequest request;
    private final AppUserState userState;
    private final WebDAVConnection connection;

    public WebDAVUpdateFile(final WebDAVRequest request, final WebDAVConnection connection) {
        this.request = request;
        this.userState = request.getUserState();
        this.connection = connection;
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final String charset = userState.getCharset();
        //String text = new XedActionFileEdit(request.getLocale()).getFile();  // filter POST data through document
        final String content = httpArguments.getValue("fileEdit.fileEditType.file");  // just grab from POST data
        final byte[] bytes = UTF8Codec.toBytes(content, charset);
        final int length = ByteU.length(bytes);
        // put data to remote
        final WebDAVDataSource source = new WebDAVDataSource(request, connection);
        source.write(bytes, connection.getURL() + request.getPathURL());
        // info alert
        final String hash = HexCodec.encode(HashU.md5(bytes));
        request.getAlerts().add(new Alert(Alert.Severity.INFO, request.getBundle().format(
                "SFTPHandlerPostMultipart.file.target", request.getPath(), length, hash)));
    }
}
