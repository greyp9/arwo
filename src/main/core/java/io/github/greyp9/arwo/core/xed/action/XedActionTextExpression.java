package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Locale;
import java.util.Properties;

public class XedActionTextExpression extends XedAction {
    private final Locale locale;

    public XedActionTextExpression(final XedFactory factory, final Locale locale) throws IOException {
        super(App.Actions.QNAME_TEXT_EXPRESSION, factory, null);
        this.locale = locale;
    }

    public final void addContentTo(
            final Element html, final String submitID, final Properties properties) throws IOException {
        final boolean isToggle = PropertiesU.isBoolean(properties, App.Action.TEXT_EXPRESSION);
        if (isToggle) {
            // view (form submit buttons)
            final Xed xedUI = getXedUI(locale);
            final XedPropertyPageView pageView = new XedPropertyPageView(null, new XedNav(xedUI).getRoot());
            final Bundle bundle = xedUI.getBundle();
            final ActionFactory factory = new ActionFactory(
                    submitID, bundle, App.Target.USER_STATE, App.Action.TEXT_EXPRESSION, null);
            final Collection<String> actions = CollectionU.toCollection(App.Action.TEXT_EXPRESSION);
            final ActionButtons buttons = factory.create(App.Action.TEXT_EXPRESSION, false, actions);
            new PropertyStripHtmlView(pageView, buttons).addContentDiv(html);
        }
    }

    public final void updateTextFilters(
            final TextFilters textFilters, final NameTypeValues httpArguments) throws IOException {
        final Xed xed = super.update(httpArguments);
        final XPather xpather = xed.getXPather();
        final String expression = xpather.getText("/action:textExpression/action:expression");  // i18n xpath
        final boolean isExpression = (!Value.isEmpty(expression));
        textFilters.getExpressions().clear();
        if (isExpression) {
            textFilters.getExpressions().add(expression);
        } else {
            textFilters.getExpressions().clear();
        }
    }
}
