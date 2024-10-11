package io.github.greyp9.arwo.app.core.view.rename;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.xed.action.XedAction;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyPageHtmlView;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import javax.xml.namespace.QName;

/**
 * XHTML (xed) for "rename files" action.
 */
public class AppFilesRenameView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppFilesRenameView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }


    public final HttpResponse addContentTo(final Element html) throws IOException {
        final XedAction action = new XedAction(App.Actions.QNAME_FILES_RENAME, userState.getXedFactory(), null);
        final Xed xedUI = action.getXedUI(userState.getLocus().getLocale());
        final XedCursor cursor = new XedNav(xedUI).getRoot();

        final Bundle bundle = cursor.getXed().getBundle();
        final QName qname = cursor.getTypeInstance().getQName();
        final String cursorType = qname.toString();
        final String submitID = userState.getSubmitID();
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.SESSION, cursorType, null);
        final Collection<String> actions = CollectionU.toCollection(App.Action.FILES_RENAME);
        final ActionButtons buttons = factory.create(qname.getLocalPart(), false, actions);
        final XedRequest xedRequest = new XedRequest(httpRequest, null, userState.getDocumentState());
        new PropertyPageHtmlView(new XedPropertyPageView(null, cursor, buttons), xedRequest).addContentTo(html);
        return null;
    }
}
