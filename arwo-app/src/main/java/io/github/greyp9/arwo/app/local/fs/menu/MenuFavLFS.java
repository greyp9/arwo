package io.github.greyp9.arwo.app.local.fs.menu;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public final class MenuFavLFS {
    private final String baseURIMode;
    private final AppUserState userState;

    public MenuFavLFS(final String baseURIMode, final AppUserState userState) {
        this.baseURIMode = baseURIMode;
        this.userState = userState;
    }

    public MenuItem toMenuItem() throws IOException {
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final Xed xedUI = userState.getXedFactory().getXedUI(xed, userState.getLocus().getLocale());
        final XedNav nav = new XedNav(xedUI);
        final XedCursor cursorFavorites = nav.findX("/app:favorites/app:lfsFavorites");  // i18n xpath
        final XedCursor cursorFavorite = nav.find("lfsFavorite", cursorFavorites);  // i18n xpath
        final TypeInstance typeInstance = cursorFavorite.getTypeInstance();
        final XedCursor parentConcrete = cursorFavorite.getParentConcrete();

        final List<MenuItem> menuItems = new ArrayList<>();
        final Collection<Element> children = parentConcrete.getChildren(typeInstance);
        for (final Element child : children) {
            if (Boolean.parseBoolean(ElementU.getAttribute(child, App.Settings.ENABLED))) {
                final String name = ElementU.getAttribute(child, App.Settings.NAME);
                final String folder = child.getAttribute(App.Settings.FOLDER);
                final String resource = child.getAttribute(App.Settings.RESOURCE);
                final String href = String.format("%s/%s%s", baseURIMode, folder, resource);
                menuItems.add(new MenuItem(name, null, App.Action.HREF_ABS, href, null));
            }
        }
        return new MenuItem("Favorites", App.Target.USER_STATE,
                App.Action.MENU2, "/test/menuFavorites", null, menuItems);
    }
}
