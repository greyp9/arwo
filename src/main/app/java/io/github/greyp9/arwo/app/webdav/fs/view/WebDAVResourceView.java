package io.github.greyp9.arwo.app.webdav.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.webdav.connection.WebDAVConnectionResource;
import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.app.webdav.fs.data.WebDAVDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.resource.PathU;

import java.io.IOException;

public class WebDAVResourceView {
    private final WebDAVRequest request;
    private final AppUserState userState;
    private final WebDAVConnectionResource resource;

    public WebDAVResourceView(
            final WebDAVRequest request, final AppUserState userState, final WebDAVConnectionResource resource) {
        this.request = request;
        this.userState = userState;
        this.resource = resource;
    }

    public final HttpResponse doGetResource(final String path) throws IOException {
        HttpResponse httpResponse;
        final WebDAVDataSource source = new WebDAVDataSource(request, resource.getConnection());
        final int lstat = source.lstat(request.getPathURL());
        final boolean isFolder = (lstat == App.FS.S_IFDIR);
        final boolean isFile = (lstat == App.FS.S_IFREG);
        if ((isFolder) && (!path.endsWith(Http.Token.SLASH))) {
            // folder path in application should end with slash
            httpResponse = HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getURI()));
        } else if (isFolder) {
            httpResponse = new WebDAVFolderView(request, userState, resource).doGetResponse();
        } else if (isFile) {
            httpResponse = new WebDAVFileView(request, userState, resource).doGetResponse();
        } else {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, String.format("%d = lstat()", lstat)));
            httpResponse = HttpResponseU.to501();
        }
        return httpResponse;
    }
}
