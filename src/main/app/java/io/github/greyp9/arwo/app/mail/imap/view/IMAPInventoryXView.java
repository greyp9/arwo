package io.github.greyp9.arwo.app.mail.imap.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.connect.AppConnectionView;
import io.github.greyp9.arwo.app.mail.imap.core.IMAPRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import org.w3c.dom.Element;

import java.io.IOException;

public class IMAPInventoryXView extends IMAPView {

    public IMAPInventoryXView(final IMAPRequest request, final AppUserState userState) {
        super(request, userState);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final AppUserState userState = getUserState();
        new IMAPInventoryView(httpRequest, userState, "").addContent(html);
        new AppConnectionView(httpRequest, userState, userState.getMail().getCacheIMAP(), "").addContentTo(html);
        return null;
    }
}
