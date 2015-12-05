package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import java.util.Properties;

public class XedActionLocale extends XedAction {
    private final Locale locale;

    public XedActionLocale(final Locale locale) throws IOException {
        super(App.Actions.QNAME_LOCALE, locale);
        this.locale = locale;
    }

    public final void addContentTo(
            final Element html, final String submitID, final Properties properties) throws IOException {
        final boolean isToggleLocale = Boolean.parseBoolean(properties.getProperty("locale"));
        if (isToggleLocale) {
            // model
            update(NameTypeValuesU.create("locale.localeType.locale", locale.getLanguage()));
            // view (form submit buttons)
            final XedPropertyPageView pageView = new XedPropertyPageView(null, getCursor());
            final Bundle bundle = this.getXed().getBundle();
            final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.USER_STATE, "locale", null);
            final Collection<String> actions = new ArrayList<String>();
            actions.add("updateLocale");
            final ActionButtons buttons = factory.create("locale", false, actions);
            new PropertyStripHtmlView(pageView, buttons).addContentDiv(html);
        }
    }
}
