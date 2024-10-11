package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
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
import java.util.Properties;

public class XedActionRefresh extends XedAction {
    private final Locale locale;

    public XedActionRefresh(final XedFactory factory, final Locale locale) throws IOException {
        super(App.Actions.QNAME_REFRESH, factory, locale);
        this.locale = locale;
    }

    public final void addContentTo(final Element html, final String submitID,
                                   final Properties properties) throws IOException {
        final boolean isToggle = PropertiesU.isBoolean(properties, App.Action.REFRESH);
        if (isToggle) {
            // model
            final String interval = properties.getProperty(Const.KEY);
            update(NameTypeValuesU.create(Const.KEY, interval));
            // view (form submit buttons)
            final Xed xedUI = getXedUI(locale);
            final XedPropertyPageView pageView = new XedPropertyPageView(null, new XedNav(xedUI).getRoot());
            final ActionFactory factory = new ActionFactory(
                    submitID, xedUI.getBundle(), App.Target.USER_STATE, App.Action.REFRESH, null);
            final Collection<String> actions = Collections.singletonList(App.Action.UPDATE);
            final ActionButtons buttons = factory.create(App.Action.REFRESH, false, actions);
            new PropertyStripHtmlView(pageView, buttons).addContentDiv(html);
        }
    }

    public final String getInterval(final NameTypeValues httpArguments) throws IOException {
        final Xed xed = super.update(httpArguments);
        return xed.getXPather().getText("/action:refresh/action:interval");  // i18n xpath
    }

    public static class Const {
        public static final String KEY = "refresh.refreshType.interval";
    }
}
