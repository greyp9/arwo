package io.github.greyp9.arwo.app.webdav.action;

import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.app.webdav.fs.data.WebDAVDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.WebDAVConnection;

import java.io.IOException;
import java.net.HttpURLConnection;

public class WebDAVCreateFile {
    private final WebDAVRequest request;
    private final WebDAVConnection connection;
    private final Bundle bundle;
    private final Alerts alerts;

    public WebDAVCreateFile(final WebDAVRequest request, final WebDAVConnection connection) {
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
        final FileX fileXDisplay = new FileX(Value.join("", request.getPath(), filename));
        final FileX fileX = new FileX(Value.join("", request.getPathURL(), URLCodec.encode(filename)));
        final WebDAVDataSource source = new WebDAVDataSource(request, connection);
        final int lstat = source.lstat(fileX.getPath());
        if (lstat == HttpURLConnection.HTTP_NOT_FOUND) {
            final String hash = HexCodec.encode(HashU.md5(bytes));
            source.write(bytes, fileX.getPath());
            alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                    "SFTPHandlerPostMultipart.file.target", fileXDisplay.getPath(), length, hash)));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format(
                    "SFTPHandlerPostMultipart.file.exists", fileXDisplay.getPath(), request.getServer())));
        }
    }
}
