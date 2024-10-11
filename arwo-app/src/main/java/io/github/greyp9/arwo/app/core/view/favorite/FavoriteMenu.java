package io.github.greyp9.arwo.app.core.view.favorite;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;

public class FavoriteMenu {
    private final LFSRequest request;
    private final AppUserState userState;
    private final XedCursor cursorFavorites;
    private final String type;

    public FavoriteMenu(final LFSRequest request, final AppUserState userState,
                        final XedCursor cursorFavorites, final String type) {
        this.request = request;
        this.userState = userState;
        this.cursorFavorites = cursorFavorites;
        this.type = type;
    }

    public final void addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final String menuKey = Value.join(Http.Token.SLASH,
                httpRequest.getServletPath(), type, AppMenuFactory.Const.FAVORITES);
        if ((cursorFavorites != null) && (userState.getMenuSystem().isOpen(menuKey))) {
            addContentToInner(html);
        }
    }

    private void addContentToInner(final Element html) {
        final Element divMenu = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
        ElementU.addElement(divMenu, Html.SPAN, "[Favorites]", NTV.create(Html.CLASS, App.CSS.MENU));
        final TypeInstance typeInstance = cursorFavorites.getTypeInstance();
        final XedCursor parentConcrete = cursorFavorites.getParentConcrete();
        final Collection<Element> children = parentConcrete.getChildren(typeInstance);
        for (final Element child : children) {
            final String enabled = ElementU.getAttribute(child, App.Settings.ENABLED);
            final String comment = ElementU.getAttribute(child, App.Settings.COMMENT);
            final String name = ElementU.getAttribute(child, App.Settings.NAME, comment);
            final String folder = child.getAttribute(App.Settings.FOLDER);
            final String resource = child.getAttribute(App.Settings.RESOURCE);
            if (Boolean.parseBoolean(enabled)) {
                final String href = String.format("%s/%s%s", request.getBaseURIMode(), folder, resource);
                ElementU.addElement(divMenu, Html.A, name,
                        NTV.create(Html.CLASS, App.CSS.MENU, Html.HREF, href, Html.TITLE, comment));
            }
        }
    }
}
