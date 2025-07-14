package io.github.greyp9.arwo.app.local.sh2.view.demo;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyPageHtmlView;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;

public final class TextAreaLSH {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public TextAreaLSH(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public void addTextArea(final Element html, final String command) throws IOException {
        final XedActionCommand action = new XedActionCommand(userState.getXedFactory());
        final Xed xedUI = action.getXedUI(userState.getLocale());
        final XedCursor cursor = new XedNav(xedUI).getRoot();
        final Bundle bundle = cursor.getXed().getBundle();
        final NameTypeValues ntv = NameTypeValuesU.create("command.commandType.command", command);
        final ValueInstance valueInstanceIn = ValueInstance.create(cursor.getTypeInstance(), ntv);
        new OpUpdate(null, action.getXed()).apply(cursor.getElement(), valueInstanceIn);

        final String qname = cursor.getTypeInstance().getQName().toString();
        final String submitID = userState.getSubmitID();
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.SESSION, qname, null);
        final Collection<String> collection = Collections.singleton(App.Action.COMMAND);
        final ActionButtons buttons = factory.create(App.Action.COMMAND, false, collection);
        final XedRequest xedRequest = new XedRequest(httpRequest, null, userState.getDocumentState());
        new PropertyPageHtmlView(new XedPropertyPageView(null, cursor, buttons), xedRequest).addContentTo(html);
    }
}
