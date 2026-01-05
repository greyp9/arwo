package io.github.greyp9.arwo.core.menu2.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public final class MenuHtml {
    private final ServletHttpRequest httpRequest;
    private final Bundle bundle;
    private final String submitID;
    private final String styleHome;

    public MenuHtml(final ServletHttpRequest httpRequest,
                    final Bundle bundle,
                    final String submitID,
                    final String styleHome) {
        this.httpRequest = httpRequest;
        this.bundle = bundle;
        this.submitID = submitID;
        this.styleHome = styleHome;
    }

    public Element addTo(final Element html,
                         final boolean home,
                         final String accessKey,
                         final List<MenuItem> menuItems) {
        final MenuItem menuItem1 = menuItems.stream().findFirst().orElse(null);
        final List<MenuItem> menuItemsNN = menuItems.stream().filter(Objects::nonNull).collect(Collectors.toList());
        for (MenuItem menuItem : menuItemsNN) {
            addTo(html, home, accessKey, menuItem, menuItem.equals(menuItem1));
        }
        return html;
    }

    private Element addTo(final Element html,
                          final boolean home,
                          final String accessKey,
                          final MenuItem menuItem,
                          final boolean first) {
        final Element divMenus = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENUS));
        final NameTypeValues attrs = NTV.create(Html.METHOD, Html.POST, Html.ACTION, Html.EMPTY);
        final Element form = ElementU.addElement(divMenus, Html.FORM, null, attrs);
        final Element divForm = ElementU.addElement(form, Html.DIV);
        addMenu(divForm, menuItem, home, first, accessKey, null);
        return divMenus;
    }

    private void addHome(final Element html) {
        final Element divNav = ElementU.addElement(html, Html.DIV, null, NTV.create(
                Html.STYLE, styleHome,
                Html.CLASS, App.CSS.RIGHT));
        final String title = bundle.getString("menu.home.DETAIL");
        final String label = String.format("[%s]", UTF16.HOME);
        ElementU.addElement(divNav, Html.A, label, NTV.create(Html.TITLE, title,
                Html.CLASS, App.CSS.MENU, Html.HREF, httpRequest.getContextPath()));
    }

    private void addMenu(final Element html, final MenuItem item, final boolean home,
                         final boolean topRow, final String accessKey, final String text) {
        final Element divMenu = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
        final String key = Value.join(Http.Token.DOT, App.CSS.MENU, item.getName());
        final String title = (bundle == null) ? null : bundle.getString(key + Bundle.DETAIL);
        final String label = String.format("[%s]", (bundle == null)
                ? item.getName() : bundle.getString(key, item.getName()));
        if (topRow) {
            Value.doIf(home, () -> addHome(divMenu));
            final SubmitToken token = new SubmitToken(
                    item.getSubject(), item.getAction(), item.getObject(), item.getObject2());
            HtmlU.addButton(divMenu, label, submitID,
                    token.toString(), App.CSS.MENU, title, accessKey);
        } else {
            ElementU.addElement(divMenu, Html.SPAN, label, NTV.create(Html.CLASS, App.CSS.MENU));
        }
        if (item.isOpen()) {
            addMenuItems(html, divMenu, item, accessKey);
        }
        if (text != null) {
            final Element divRight = ElementU.addElement(divMenu, Html.DIV, null,
                    NTV.create(Html.CLASS, App.CSS.RIGHT));
            ElementU.addElement(divRight, Html.SPAN, text, NTV.create(Html.CLASS, App.CSS.MENU));
        }
    }

    private void addMenuItems(final Element html, final Element divMenu, final MenuItem item, final String accessKey) {
        MenuItem itemOpen = null;
        for (final MenuItem itemIt : item.getMenuItems()) {
            itemOpen = ((itemIt.isOpen()) ? itemIt : itemOpen);
            final String parentName = (item.getName().equals(UTF16.MENU) ? null : item.getName());
            final String key = Value.join(".", App.CSS.MENU, parentName, itemIt.getName());
            final String title = (bundle == null) ? null : bundle.getString(key + Bundle.DETAIL);
            final String label = (bundle == null) ? itemIt.getName() : bundle.getString(key);
            if (itemIt.getAction().equals(App.Action.HREF)) {
                final String baseURI = httpRequest.getBaseURI();
                final String pathInfo = httpRequest.getPathInfo();
                final String object = itemIt.getObject();
                final String pathInfo1 = getPathInfo(pathInfo, object);
                ElementU.addElement(divMenu, Html.A, label, NTV.create(Html.TITLE, title,
                        Html.CLASS, App.CSS.MENU, Html.HREF, (baseURI + pathInfo1)));
            } else if (itemIt.getAction().equals(App.Action.HREF_ABS)) {
                final String name = itemIt.getName();
                final String object = itemIt.getObject();
                ElementU.addElement(divMenu, Html.A, name, NTV.create(Html.TITLE, title,
                        Html.CLASS, App.CSS.MENU, Html.HREF, object));
            } else {
                final SubmitToken token = new SubmitToken(
                        itemIt.getSubject(), itemIt.getAction(), itemIt.getObject(), itemIt.getObject2());
                final String htmlClass = Value.join(Html.SPACE, App.CSS.MENU, App.CSS.MIN,
                        (itemIt.isOpen() ? App.CSS.ACTIVE : null));
                HtmlU.addButton(divMenu, label, submitID, token.toString(),
                        htmlClass, title, accessKey);
            }
        }
        if ((itemOpen != null) && (!itemOpen.getMenuItems().isEmpty())) {
            addMenu(html, itemOpen, false, false, accessKey, null);
        }
    }

    private String getPathInfo(final String pathInfo, final String contextUpdate) {
        final Pather pather0 = new Pather(pathInfo);
        return Http.Token.SLASH + contextUpdate + pather0.getRight();
    }
}
