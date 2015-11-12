package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;

public class XedActionLocale extends XedAction {

    public XedActionLocale(final Locale locale) throws IOException {
        super(App.Actions.QNAME_LOCALE, locale);
    }

    public final void addContentTo(
            final Element html, final String localeID, final String submitID) throws IOException {
        final boolean showLocaleStrip = SystemU.isTrue();
        if (showLocaleStrip) {
            // model
            update(NameTypeValuesU.create("locale.localeType.locale", localeID));
            // view (form submit buttons)
            final XedPropertyPageView pageView = new XedPropertyPageView(null, getCursor());
            final Bundle bundleXed = this.getXed().getBundle();
            final ActionFactory factory = new ActionFactory(
                    submitID, bundleXed, App.Target.USER_STATE, "locale", null);
            final Collection<String> actions = new ArrayList<String>();
            actions.add(App.Action.UPDATE);
            final ActionButtons buttons = factory.create(null, actions);
            new PropertyStripHtmlView(pageView, buttons).addContentDiv(html);
        }
    }
}
