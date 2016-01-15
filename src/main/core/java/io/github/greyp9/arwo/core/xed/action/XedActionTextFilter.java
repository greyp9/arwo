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
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.Locale;
import java.util.Properties;

public class XedActionTextFilter extends XedAction {

    public XedActionTextFilter(final Locale locale) throws IOException {
        super(App.Actions.QNAME_TEXT_FILTER, locale);
    }

    public final void addContentTo(
            final Element html, final String submitID, final Properties properties) throws IOException {
        final boolean isToggle = PropertiesU.isBoolean(properties, App.Action.TEXT_FILTER);
        if (isToggle) {
            // view (form submit buttons)
            final XedPropertyPageView pageView = new XedPropertyPageView(null, getCursor());
            final Bundle bundle = this.getXed().getBundle();
            final ActionFactory factory = new ActionFactory(
                    submitID, bundle, App.Target.USER_STATE, App.Action.TEXT_FILTER, null);
            final Collection<String> actions = CollectionU.toCollection(App.Action.TEXT_FILTER);
            final ActionButtons buttons = factory.create(App.Action.TEXT_FILTER, false, actions);
            new PropertyStripHtmlView(pageView, buttons).addContentDiv(html);
        }
    }

    public final void updateTextFilters(
            final TextFilters textFilters, final NameTypeValues httpArguments) throws IOException {
        final Xed xed = super.update(httpArguments);
        final XPather xpather = xed.getXPather();
        final String include = xpather.getText("/action:textFilter/action:include");  // i18n
        final String exclude = xpather.getText("/action:textFilter/action:exclude");  // i18n
        final boolean isInclude = (!Value.isEmpty(include));
        final boolean isExclude = (!Value.isEmpty(exclude));
        if (isInclude) {
            textFilters.getIncludes().add(include);
        } else if (isExclude) {
            textFilters.getExcludes().add(exclude);
        } else {
            textFilters.getIncludes().clear();
            textFilters.getExcludes().clear();
        }
    }
}
