package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import java.util.Properties;

public class XedActionCommit extends XedAction {

    public XedActionCommit(final Locale locale) throws IOException {
        super(App.Actions.QNAME_COMMIT, locale);
    }

    public final String getComment(final NameTypeValues nameTypeValues) throws IOException {
        final Xed xed = super.update(nameTypeValues);
        // query xed instance for parameters
        final XPather xpather = new XPather(xed.getDocument(), xed.getXsdTypes().getContext());
        return xpather.getText("/action:commit/action:comment");
    }

    public final void addContentTo(
            final Element html, final String submitID, final Properties properties) throws IOException {
        final boolean isToggle = Boolean.parseBoolean(properties.getProperty(App.Action.COMMIT));
        if (isToggle) {
            // view (form submit buttons)
            final XedPropertyPageView pageView = new XedPropertyPageView(null, getCursor());
            final Bundle bundle = this.getXed().getBundle();
            final ActionFactory factory = new ActionFactory(
                    submitID, bundle, App.Target.SESSION, App.Action.COMMIT, null);
            final Collection<String> actions = new ArrayList<String>();
            actions.add(App.Action.COMMIT);
            final ActionButtons buttons = factory.create(App.Action.COMMIT, actions);
            new PropertyStripHtmlView(pageView, buttons).addContentDiv(html);
        }
    }
}
