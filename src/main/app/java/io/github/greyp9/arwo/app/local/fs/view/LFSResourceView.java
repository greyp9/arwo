package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.resource.PathU;

import java.io.File;
import java.io.IOException;

public class LFSResourceView {
    private final LFSRequest request;
    private final AppUserState userState;
    private final File folderBase;

    public LFSResourceView(final LFSRequest request, final AppUserState userState, final File folderBase) {
        this.request = request;
        this.userState = userState;
        this.folderBase = folderBase;
    }

    public final HttpResponse doGetResource(final String path) throws IOException {
        HttpResponse httpResponse;
        final LFSDataSource source = new LFSDataSource(request, folderBase);
        final boolean isFolderPath = path.endsWith(Http.Token.SLASH);
        final boolean isZipPath = path.contains("!/");
        final File file = isZipPath
                ? source.getFile(path.substring(0, path.indexOf("!/")))
                : source.getFile(path);
        final boolean isFolder = file.isDirectory();
        final boolean isFile = file.isFile();
        final boolean isLink = FileU.isLink(file);
        final boolean exists = file.exists();
        if (isFolder && (!isFolderPath)) {
            // folder path in application should end with slash
            httpResponse = HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getURI()));
        } else if (isLink) {
            httpResponse = new LFSSymlinkView(request, userState, folderBase, file).doGetResponse();
        } else if (isFolder) {
            httpResponse = new LFSFolderView(request, userState, folderBase, file).doGetResponse();
        } else if (isFile) {
            httpResponse = new LFSFileView(request, userState, folderBase, file).doGetResponse();
        } else if (exists) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, path));
            httpResponse = HttpResponseU.to302(PathU.toParent(request.getHttpRequest().getURI()));
        } else {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, path));
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }
}
