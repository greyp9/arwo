package io.github.greyp9.arwo.app.mail.pop3.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.connect.AppConnectionView;
import io.github.greyp9.arwo.app.mail.pop3.core.POP3Request;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import org.w3c.dom.Element;

import java.io.IOException;

public class POP3InventoryXView extends POP3View {

    public POP3InventoryXView(final POP3Request request, final AppUserState userState) {
        super(request, userState);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final AppUserState userState = getUserState();
        new POP3InventoryView(httpRequest, userState, "").addContent(html);
        new AppConnectionView(httpRequest, userState, userState.getMail().getCachePOP3(), "").addContentTo(html);
        return null;
    }
}
