package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Locale;

public class XedActionProtect extends XedAction {
    private final Locale locale;

    public XedActionProtect(final XedFactory factory, final Locale locale) throws IOException {
        super(App.Actions.QNAME_PROTECT, factory, locale);
        this.locale = locale;
    }

    public final void addContentTo(final Element html, final String submitID) throws IOException {
        final Xed xedUI = getXedUI(locale);
        final XedPropertyPageView pageView = new XedPropertyPageView(null, new XedNav(xedUI).getRoot());
        final ActionFactory factory = new ActionFactory(
                submitID, xedUI.getBundle(), App.Target.USER_STATE, App.Action.PROTECT, null);
        final Collection<String> actions = Collections.singletonList(App.Action.PROTECT);
        final ActionButtons buttons = factory.create(App.Action.PROTECT, false, actions);
        new PropertyStripHtmlView(pageView, buttons).addContentDiv(html);
    }

    public final String getProtect(final NameTypeValues httpArguments) throws IOException {
        final Xed xed = super.update(httpArguments);
        return xed.getXPather().getText("/action:protect/action:input");  // i18n xpath
    }

    public static class Const {
        public static final String KEY = "protect.protectType.input";
    }
}
