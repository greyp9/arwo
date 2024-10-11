package io.github.greyp9.arwo.core.html.menu;

import io.github.greyp9.arwo.core.action.ActionItem;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Collection;

public class ActionItemsMenu {
    private final String label;
    private final String baseURI;
    private final Collection<ActionItem> items;

    public ActionItemsMenu(final String label, final String baseURI, final Collection<ActionItem> items) {
        this.label = label;
        this.baseURI = baseURI;
        this.items = items;
    }

    public final void addContentTo(final Element html) {
        final Element divMenu = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
        ElementU.addElement(divMenu, Html.SPAN, label, NTV.create(Html.CLASS, App.CSS.MENU));
        for (final ActionItem item : items) {
            final String href = String.format("%s/%s", baseURI, item.getName());
            ElementU.addElement(divMenu, Html.A, item.getName(),
                    NTV.create(Html.CLASS, App.CSS.MENU, Html.HREF, href, Html.TITLE, item.getName()));
        }
    }
}
