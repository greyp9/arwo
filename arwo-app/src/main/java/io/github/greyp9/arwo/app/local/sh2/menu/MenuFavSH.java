package io.github.greyp9.arwo.app.local.sh2.menu;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;
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
import java.util.stream.Collectors;

public final class MenuFavSH {
    private final String baseURI;
    private final AppUserState userState;

    public MenuFavSH(final String baseURI, final AppUserState userState) {
        this.baseURI = baseURI;
        this.userState = userState;
    }

    public MenuItem toMenuItem() throws IOException {
        // enumerate commands
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final Xed xedUI = userState.getXedFactory().getXedUI(xed, userState.getLocus().getLocale());
        final XedNav nav = new XedNav(xedUI);
        final XedCursor cursorFavorites = nav.findX("/app:favorites/app:lshFavorites");  // i18n xpath
        final XedCursor cursorFavorite = nav.find("lshFavorite", cursorFavorites);  // i18n xpath
        final TypeInstance typeInstance = cursorFavorite.getTypeInstance();
        final XedCursor parentConcrete = cursorFavorite.getParentConcrete();
        final Collection<NameTypeValue> favorites = new ArrayList<>();
        final Collection<Element> children = parentConcrete.getChildren(typeInstance);
        for (final Element child : children) {
            if (Boolean.parseBoolean(ElementU.getAttribute(child, App.Settings.ENABLED))) {
                final String context = ElementU.getAttribute(child, App.Settings.CONTEXT);
                final String name = ElementU.getAttribute(child, App.Settings.NAME);
                if (!Value.isEmpty(context)) {
                    favorites.add(new NameTypeValue(name, context, null));
                }
            }
        }

        // group commands by context
        final List<MenuItem> menuItemsC = new ArrayList<>();
        final List<String> contexts = favorites.stream()
                .map(NameTypeValue::getType).distinct().collect(Collectors.toList());
        for (String context : contexts) {
            final List<String> names = favorites.stream()
                    .filter(ntv -> ntv.getType().equals(context))
                    .map((NameTypeValue::getName)).collect(Collectors.toList());
            final List<MenuItem> menuItemsN = new ArrayList<>();
            for (String name : names) {
                menuItemsN.add(new MenuItem(name, App.Target.SESSION, App.Action.SELECT_FAV, context, name));
            }
            menuItemsC.add(new MenuItem(context, App.Target.USER_STATE, App.Action.MENU2,
                    PathU.toPath(MENU_STATE_BASE, context), PathU.toDir(baseURI, context), menuItemsN));
        }
        return new MenuItem("Favorites", App.Target.USER_STATE, App.Action.MENU2, MENU_STATE_BASE, null, menuItemsC);
    }

    private static final String MENU_STATE_BASE = "/menu2/sh/favorites";
}
