package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.resource.PathU;

import java.io.File;
import java.io.IOException;

public class LFSResourceView {
    private final LFSRequest request;
    private final AppUserState userState;

    public LFSResourceView(final LFSRequest request, final AppUserState userState) {
        this.request = request;
        this.userState = userState;
    }

    public final HttpResponse doGetResource(final String path) throws IOException {
        HttpResponse httpResponse;
        final LFSDataSource source = new LFSDataSource(request, userState.getUserRoot());
        final File file = source.getFile(path);
        final boolean isFolder = file.isDirectory();
        final boolean isFile = file.isFile();
        if ((isFolder) && (!path.endsWith(Http.Token.SLASH))) {
            // folder path in application should end with slash
            httpResponse = HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getURI()));
        } else if (isFolder) {
            httpResponse = new LFSFolderView(request, userState, file).doGetResponse();
        } else if (isFile) {
            httpResponse = new LFSFileView(request, userState, file).doGetResponse();
        } else {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, path));
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }
}
