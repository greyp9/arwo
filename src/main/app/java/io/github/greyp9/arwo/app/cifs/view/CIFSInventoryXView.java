package io.github.greyp9.arwo.app.cifs.view;

import io.github.greyp9.arwo.app.cifs.connection.CIFSConnectionResource;
import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.connect.AppConnectionView;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import org.w3c.dom.Element;

import java.io.IOException;

public class CIFSInventoryXView extends CIFSView {
    private final String offsetURI;

    public CIFSInventoryXView(final CIFSRequest request, final AppUserState userState,
                              final CIFSConnectionResource resource, final String offsetURI) {
        super(request, userState, resource);
        this.offsetURI = offsetURI;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final AppUserState userState = getUserState();
        new CIFSInventoryView(httpRequest, userState, offsetURI).addContent(html);
        new AppConnectionView(httpRequest, userState, userState.getCIFS().getCache(), offsetURI).addContentTo(html);
        return null;
    }
}
