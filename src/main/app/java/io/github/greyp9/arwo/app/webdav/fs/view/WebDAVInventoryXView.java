package io.github.greyp9.arwo.app.webdav.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.webdav.connection.WebDAVConnectionResource;
import io.github.greyp9.arwo.app.webdav.fs.core.WebDAVRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import org.w3c.dom.Element;

import java.io.IOException;

public class WebDAVInventoryXView extends WebDAVView {
    private final String offsetURI;

    public WebDAVInventoryXView(final WebDAVRequest request, final AppUserState userState,
                                final WebDAVConnectionResource resource, final String offsetURI) {
        super(request, userState, resource);
        this.offsetURI = offsetURI;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final AppUserState userState = getUserState();
        new WebDAVInventoryView(httpRequest, userState, offsetURI).addContent(html);
        new WebDAVConnectionsView(httpRequest, userState, offsetURI).addContent(html);
        return null;
    }
}
