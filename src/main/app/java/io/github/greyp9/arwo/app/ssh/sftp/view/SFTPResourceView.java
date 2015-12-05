package io.github.greyp9.arwo.app.ssh.sftp.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPDataSource;
import io.github.greyp9.arwo.app.ssh.sftp.data.SFTPFolder;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.lib.ganymed.ssh.core.SFTP;

import java.io.IOException;

public class SFTPResourceView {
    private final SFTPRequest request;
    private final AppUserState userState;
    private final SSHConnectionResource resource;

    public SFTPResourceView(
            final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        this.request = request;
        this.userState = userState;
        this.resource = resource;
    }

    public final HttpResponse doGetResource(final String path) throws IOException {
        HttpResponse httpResponse;
        final SFTPDataSource source = new SFTPDataSource(request, resource.getSSHConnection());
        final Integer type = SFTPFolder.toType(source.lstat(path));
        final boolean isFolder = type.equals(SFTP.S_IFDIR);
        final boolean isFile = type.equals(SFTP.S_IFREG);
        final boolean isLink = type.equals(SFTP.S_IFLNK);
        if ((isFolder) && (!path.endsWith(Http.Token.SLASH))) {
            // folder path in application should end with slash
            httpResponse = HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getURI()));
        } else if (isFolder) {
            httpResponse = new SFTPFolderView(request, userState, resource).doGetResponse();
        } else if (isFile) {
            httpResponse = new SFTPFileView(request, userState, resource).doGetResponse();
        } else if (isLink) {
            httpResponse = new SFTPSymlinkView(request, userState, resource).doGetResponse();
        } else {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, String.format("%d = lstat()", type)));
            httpResponse = HttpResponseU.to501();
        }
        return httpResponse;
    }
}
