package io.github.greyp9.arwo.app.core.view.create;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.xed.action.XedAction;
import org.w3c.dom.Element;

import java.io.IOException;

public class AppFolderCreateView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppFolderCreateView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(final Element html) throws IOException {
        final XedAction action = new XedAction(
                App.Actions.QNAME_FOLDER_NEW, userState.getXedFactory(), userState.getLocale());
        action.addPropertyPageTo(html, userState.getSubmitID(), httpRequest, userState.getDocumentState(),
                CollectionU.toCollection(App.Action.FOLDER_CREATE));
        return null;
    }
}
