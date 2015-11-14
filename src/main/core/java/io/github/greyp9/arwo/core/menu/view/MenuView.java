package io.github.greyp9.arwo.core.menu.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;

public class MenuView {
    private final Bundle bundle;
    private final ServletHttpRequest httpRequest;
    private final MenuSystem menuSystem;

    public MenuView(final Bundle bundle, final ServletHttpRequest httpRequest, final MenuSystem menuSystem) {
        this.bundle = bundle;
        this.httpRequest = httpRequest;
        this.menuSystem = menuSystem;
    }

    public final void addContentTo(final Element html, final String type, final boolean home) throws IOException {
        final MenuItem menuItem = menuSystem.get(httpRequest.getServletPath(), type);
        final Element divMenus = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENUS));
        final NameTypeValues attrs = NTV.create(Html.METHOD, Html.POST, Html.ACTION, "");
        final Element form = ElementU.addElement(divMenus, Html.FORM, null, attrs);
        final Element divForm = ElementU.addElement(form, Html.DIV);
        addMenu(divForm, menuItem, home, true);
    }

    private void addMenu(final Element html, final MenuItem item, final boolean home, final boolean top) {
        final Element divMenu = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
        if (home) {
            addHome(divMenu);
        }
        final String key = Value.join(".", App.CSS.MENU, item.getName());
        final String label = String.format("[%s]", bundle.getString(key, item.getName()));
        if (top) {
            final SubmitToken token = new SubmitToken(item.getSubject(), item.getAction(), item.getObject());
            HtmlU.addButton(divMenu, label, menuSystem.getSubmitID(), token.toString(), App.CSS.MENU, null);
        } else {
            ElementU.addElement(divMenu, Html.SPAN, label, NTV.create(Html.CLASS, App.CSS.MENU));
        }
        if (item.isOpen()) {
            addMenuItems(html, divMenu, item);
        }
    }

    private void addHome(final Element html) {
        final Element divNav = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.STYLE, "float: right;"));
        final String label = String.format("[%s]", UTF16.HOME);
        ElementU.addElement(divNav, Html.A, label, NTV.create(
                Html.CLASS, "menu", Html.HREF, httpRequest.getContextPath()));
    }

    @SuppressWarnings({ "PMD.NPathComplexity", "PMD.AvoidInstantiatingObjectsInLoops" })
    private void addMenuItems(final Element html, final Element divMenu, final MenuItem item) {
        MenuItem itemOpen = null;
        for (final MenuItem itemIt : item.getMenuItems()) {
            itemOpen = ((itemIt.isOpen()) ? itemIt : itemOpen);
            final String parentName = (item.getName().equals(UTF16.MENU) ? null : item.getName());
            final String key = Value.join(".", App.CSS.MENU, parentName, itemIt.getName());
            final String label = bundle.getString(key, itemIt.getName());
            final SubmitToken token = new SubmitToken(itemIt.getSubject(), itemIt.getAction(), itemIt.getObject());
            final String htmlClass = ((itemIt.isOpen()) ? "menu min active" : "menu min");
            HtmlU.addButton(divMenu, label, menuSystem.getSubmitID(), token.toString(), htmlClass, null);
        }
        if ((itemOpen != null) && (!itemOpen.getMenuItems().isEmpty())) {
            addMenu(html, itemOpen, false, false);
        }
    }
}
