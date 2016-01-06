package io.github.greyp9.arwo.app.webdav.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.connect.AppConnectionView;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import org.w3c.dom.Element;

import java.io.IOException;

public class WebDAVConnectionsView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String offsetURI;

    public WebDAVConnectionsView(
            final ServletHttpRequest httpRequest, final AppUserState userState, final String offsetURI) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.offsetURI = offsetURI;
    }

    public final void addContent(final Element html) throws IOException {
        final String baseURI = PathU.toPath(httpRequest.getBaseURI(), offsetURI);
        final ConnectionCache cache = userState.getWebDAV().getCache();
        new AppConnectionView(httpRequest, userState, cache, baseURI).addContentTo(html);
    }
}
