package io.github.greyp9.arwo.app.ssh.core.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.connect.AppConnectionView;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import org.w3c.dom.Element;

import java.io.IOException;

public class SSHConnectionsView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String offsetURI;

    public SSHConnectionsView(
            final ServletHttpRequest httpRequest, final AppUserState userState, final String offsetURI) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.offsetURI = offsetURI;
    }

    public final void addContent(final Element html) throws IOException {
        final String baseURI = PathU.toPath(httpRequest.getBaseURI(), offsetURI);
        final ConnectionCache cache = userState.getSSH().getCache();
        new AppConnectionView(httpRequest, userState, cache, baseURI).addContentTo(html, true);
    }
}
