package io.github.greyp9.arwo.app.ssh.sftp.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.core.view.SSHConnectionView;
import io.github.greyp9.arwo.app.ssh.core.view.SSHInventoryView;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import org.w3c.dom.Element;

import java.io.IOException;

public class SFTPInventoryView extends SFTPView {
    private final String offsetURI;

    public SFTPInventoryView(final SFTPRequest request, final AppUserState userState,
                             final SSHConnectionResource resource, final String offsetURI) {
        super(request, userState, resource);
        this.offsetURI = offsetURI;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final AppUserState userState = getUserState();
        new SSHInventoryView(httpRequest, userState, offsetURI).addContent(html);
        new SSHConnectionView(httpRequest, userState, offsetURI, true).addContent(html);
        return null;
    }
}
