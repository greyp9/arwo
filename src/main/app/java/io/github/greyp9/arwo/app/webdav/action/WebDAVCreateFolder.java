package io.github.greyp9.arwo.app.webdav.action;

import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.app.webdav.fs.data.WebDAVDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.lib.sardine.webdav.connection.WebDAVConnection;

import java.io.IOException;
import java.net.HttpURLConnection;

public class WebDAVCreateFolder {
    private final WebDAVRequest request;
    private final WebDAVConnection connection;
    private final Bundle bundle;
    private final Alerts alerts;

    public WebDAVCreateFolder(final WebDAVRequest request, final WebDAVConnection connection) {
        this.request = request;
        this.connection = connection;
        this.bundle = request.getBundle();
        this.alerts = request.getAlerts();
    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final String filename = httpArguments.getValue("folderNew.folderNewType.filename");
        // send remote command
        final FileX fileXDisplay = new FileX(Value.join("", request.getPath(), filename));
        final FileX fileX = new FileX(Value.join("", request.getPathURL(), URLCodec.encode(filename)));
        final WebDAVDataSource source = new WebDAVDataSource(request, connection);

        final int lstat = source.lstat(fileX.getPath());
        if (lstat == HttpURLConnection.HTTP_NOT_FOUND) {
            source.createDirectory(fileX.getPath());
            alerts.add(new Alert(Alert.Severity.INFO, bundle.format(
                    "SFTPHandlerPostMultipart.folder.target", fileXDisplay.getPath())));
        } else {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format(
                    "SFTPHandlerPostMultipart.file.exists", fileXDisplay.getPath(), request.getServer())));
        }
    }
}
