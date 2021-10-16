package io.github.greyp9.arwo.core.menu.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.http.Http;
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

    public final MenuSystem getMenuSystem() {
        return menuSystem;
    }

    public MenuView(final Bundle bundle, final ServletHttpRequest httpRequest, final MenuSystem menuSystem) {
        this.bundle = bundle;
        this.httpRequest = httpRequest;
        this.menuSystem = menuSystem;
    }

    public final Element addContentTo(final Element html, final String type, final boolean home) throws IOException {
        return addContentTo(html, type, null, home, true, null);
    }

    public final Element addContentTo(final Element html, final String type, final String object2, final boolean home,
                                      final boolean top, final String accessKey) throws IOException {
        return addContentTo(html, type, object2, home, top, accessKey, null);
    }

    public final Element addContentTo(final Element html, final String type, final String object2, final boolean home,
                                      final boolean top, final String accessKey, final String text) throws IOException {
        final MenuItem menuItem = menuSystem.get(httpRequest.getServletPath(), type, object2);
        final Element divMenus = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENUS));
        final NameTypeValues attrs = NTV.create(Html.METHOD, Html.POST, Html.ACTION, Html.EMPTY);
        final Element form = ElementU.addElement(divMenus, Html.FORM, null, attrs);
        final Element divForm = ElementU.addElement(form, Html.DIV);
        addMenu(divForm, menuItem, home, top, accessKey, text);
        return divMenus;
    }

    public final Element addTitle(final Element html, final AppTitle title) throws IOException {
        final Element divMenus = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENUS));
        final Element div = ElementU.addElement(divMenus, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.PAGE_TITLE));
        ElementU.addElement(div, Html.IMG, null,
                NTV.create(Html.SRC, "${CONTEXT}/res/ico/app-black.ico", Html.ALT, ""));
        ElementU.addElement(div, Html.SPAN, title.getText());
        return divMenus;
    }

    private void addMenu(final Element html, final MenuItem item, final boolean home,
                         final boolean top, final String accessKey, final String text) {
        final Element divMenu = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
        if (home) {
            addHome(divMenu);
        }
        final String key = Value.join(Http.Token.DOT, App.CSS.MENU, item.getName());
        final String title = bundle.getString(key + ".DETAIL");
        final String label = String.format("[%s]", bundle.getString(key, item.getName()));
        if (top) {
            final SubmitToken token = new SubmitToken(
                    item.getSubject(), item.getAction(), item.getObject(), item.getObject2());
            HtmlU.addButton(divMenu, label, menuSystem.getSubmitID(), token.toString(), App.CSS.MENU, title, accessKey);
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

    private void addHome(final Element html) {
        final Element divNav = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.RIGHT));
        final String title = bundle.getString("menu.home.DETAIL");
        final String label = String.format("[%s]", UTF16.HOME);
        ElementU.addElement(divNav, Html.A, label, NTV.create(Html.TITLE, title,
                Html.CLASS, App.CSS.MENU, Html.HREF, httpRequest.getContextPath()));
    }

    @SuppressWarnings({ "PMD.NPathComplexity", "PMD.AvoidInstantiatingObjectsInLoops" })
    private void addMenuItems(final Element html, final Element divMenu, final MenuItem item, final String accessKey) {
        MenuItem itemOpen = null;
        for (final MenuItem itemIt : item.getMenuItems()) {
            itemOpen = ((itemIt.isOpen()) ? itemIt : itemOpen);
            final String parentName = (item.getName().equals(UTF16.MENU) ? null : item.getName());
            final String key = Value.join(".", App.CSS.MENU, parentName, itemIt.getName());
            final String title = bundle.getString(key + ".DETAIL");
            final String label = bundle.getString(key);
            final SubmitToken token = new SubmitToken(
                    itemIt.getSubject(), itemIt.getAction(), itemIt.getObject(), itemIt.getObject2());
            final String htmlClass = Value.join(Html.SPACE, App.CSS.MENU, App.CSS.MIN,
                    (itemIt.isOpen() ? App.CSS.ACTIVE : null));
            HtmlU.addButton(divMenu, label, menuSystem.getSubmitID(), token.toString(), htmlClass, title, accessKey);
        }
        if ((itemOpen != null) && (!itemOpen.getMenuItems().isEmpty())) {
            addMenu(html, itemOpen, false, false, accessKey, null);
        }
    }
}
