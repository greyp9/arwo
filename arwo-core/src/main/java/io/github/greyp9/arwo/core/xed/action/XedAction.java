package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.state.XedUserState;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyPageHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.Locale;

public class XedAction {
    private final Xed xed;
    private final XedFactory factory;

    public final XedFactory getFactory() {
        return factory;
    }

    public final Xed getXed() {
        return xed;
    }

    public XedAction(final QName qname, final XedFactory factory, final Locale locale) throws IOException {
        final URL url = ResourceU.resolve(App.Actions.XSD);
        this.xed = factory.generateEmpty(url, qname, locale);
        this.factory = factory;
    }

    public final Xed getXedUI(final Locale locale) throws IOException {
        return new Xed(xed.getDocument(), xed.getXsdTypes(), factory.getXsdBundle(xed.getXsdTypes(), locale));
    }

    public final Xed update(final NameTypeValues nameTypeValues) throws IOException {
        final XedCursor cursor = new XedNav(xed).getRoot();
        final ValueInstance valueInstance = ValueInstance.create(cursor.getTypeInstance(), nameTypeValues);
        new OpUpdate(null, xed).apply(cursor.getElement(), valueInstance);
        return xed;
    }

    public final void addPropertyPageTo(final Element html,
                                        final String submitID,
                                        final ServletHttpRequest httpRequest,
                                        final XedUserState userState,
                                        final Collection<String> actions) throws IOException {
        final Xed xedUI = getXedUI(xed.getLocale());
        final XedCursor cursor = new XedNav(xedUI).getRoot();
        final Bundle bundle = cursor.getXed().getBundle();
        final QName qname = cursor.getTypeInstance().getQName();
        final String cursorType = qname.toString();
        final ActionFactory actionFactory = new ActionFactory(submitID, bundle, App.Target.SESSION, cursorType, null);
        final ActionButtons buttons = actionFactory.create(qname.getLocalPart(), false, actions);
        final XedRequest xedRequest = new XedRequest(httpRequest, null, userState);
        new PropertyPageHtmlView(new XedPropertyPageView(null, cursor, buttons), xedRequest).addContentTo(html);
    }

    public final void addPropertyStripTo(final Element html, final String submitID) throws IOException {
        final Xed xedUI = getXedUI(xed.getLocale());
        final XedPropertyPageView pageView = new XedPropertyPageView(null, new XedNav(xedUI).getRoot());
        final ActionFactory actionFactory = new ActionFactory(
                submitID, xedUI.getBundle(), App.Target.SESSION, App.Action.UPDATE, null);
        final Collection<String> actions = Collections.singletonList(App.Action.UPDATE);
        final ActionButtons buttons = actionFactory.create(App.Action.REFRESH, false, actions);
        new PropertyStripHtmlView(pageView, buttons).addContentDiv(html);
    }
}
