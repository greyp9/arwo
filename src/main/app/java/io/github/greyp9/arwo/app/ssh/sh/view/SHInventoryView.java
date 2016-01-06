package io.github.greyp9.arwo.app.ssh.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.history.AppHistoryView;
import io.github.greyp9.arwo.app.ssh.core.view.SSHConnectionsView;
import io.github.greyp9.arwo.app.ssh.core.view.SSHInventoryView;
import io.github.greyp9.arwo.app.ssh.sh.core.SHRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.History;
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
        final Bundle bundle = getRequest().getBundle();
        final AppUserState userState = getUserState();
        final History history = userState.getSSH().getHistory();
        new SSHInventoryView(httpRequest, userState, offsetURI).addContent(html);
        new SSHConnectionsView(httpRequest, userState, offsetURI).addContent(html);
        new AppHistoryView("sshHistoryType", true, history, bundle, httpRequest, userState).addContentTo(html);
        return null;
    }
}
