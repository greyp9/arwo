package io.github.greyp9.arwo.app.ssh.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.core.view.SSHConnectionView;
import io.github.greyp9.arwo.app.ssh.core.view.SSHInventoryView;
import io.github.greyp9.arwo.app.ssh.sh.core.SHRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import org.w3c.dom.Element;

import java.io.IOException;

public class SHInventoryView extends SHView {
    private final String offsetURI;

    public SHInventoryView(final SHRequest request, final AppUserState userState, final String offsetURI) {
        super(request, userState);
        this.offsetURI = offsetURI;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final AppUserState userState = getUserState();
        new SSHInventoryView(httpRequest, userState, offsetURI).addContent(html);
        new SSHConnectionView(httpRequest, userState, offsetURI).addContent(html);
        return null;
    }
}
