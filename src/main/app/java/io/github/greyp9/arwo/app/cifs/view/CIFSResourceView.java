package io.github.greyp9.arwo.app.cifs.view;

import io.github.greyp9.arwo.app.cifs.connection.CIFSConnectionResource;
import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.cifs.data.CIFSDataSource;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.resource.PathU;
import jcifs.smb.SmbFile;

import java.io.IOException;

public class CIFSResourceView {
    private final CIFSRequest request;
    private final AppUserState userState;
    private final CIFSConnectionResource resource;

    public CIFSResourceView(
            final CIFSRequest request, final AppUserState userState, final CIFSConnectionResource resource) {
        this.request = request;
        this.userState = userState;
        this.resource = resource;
    }

    public final HttpResponse doGetResource(final String path) throws IOException {
        HttpResponse httpResponse;
        final CIFSDataSource source = new CIFSDataSource(request, resource.getConnection());
        final SmbFile smbFile = source.lstat(path);
        final boolean isFolder = smbFile.isDirectory();
        final boolean isFile = smbFile.isFile();
        if ((isFolder) && (!path.endsWith(Http.Token.SLASH))) {
            // folder path in application should end with slash
            httpResponse = HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getURI()));
        } else if (isFolder) {
            httpResponse = new CIFSFolderView(request, userState, resource).doGetResponse();
        } else if (isFile) {
            httpResponse = new CIFSFileView(request, userState, resource).doGetResponse();
        } else {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, String.format("%s = lstat()", smbFile.toString())));
            httpResponse = HttpResponseU.to404();
        }
        return httpResponse;
    }
}
