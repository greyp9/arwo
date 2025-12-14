package io.github.greyp9.arwo.app.local.sh.favorite;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;
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
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class FavoriteMenuFactory implements MenuFactory {
    private final Xed xed;

    public FavoriteMenuFactory(final Xed xed) {
        this.xed = xed;
    }

    @Override
    public final MenuItem create(final String id, final String type, final String object2) {
        final Collection<NameTypeValue> nameTypeValues = getFavorites();
        final Collection<MenuItem> menuItems = new ArrayList<>();
        final List<String> contexts = nameTypeValues.stream()
                .map(NameTypeValue::getType)
                .filter(t -> !Value.isEmpty(t))
                .distinct()
                .collect(Collectors.toList());
        for (String context : contexts) {
            final Collection<MenuItem> menuItemsContext = new ArrayList<>();
            final List<String> namesForContext = nameTypeValues.stream()
                    .filter(ntv -> ntv.getType().equals(context))
                    .map(NameTypeValue::getName)
                    .collect(Collectors.toList());
            for (String name : namesForContext) {
                menuItemsContext.add(new MenuItem(name, App.Target.SESSION, App.Action.SELECT_FAV, context, name));
            }
            menuItems.add(new MenuItem(context, App.Target.USER_STATE, App.Action.MENU,
                    Value.join("/", id, type, context), context, menuItemsContext));
        }
        return new MenuItem("Favorites", App.Target.USER_STATE, App.Action.MENU, id + "/" + type, "", menuItems);
    }

    private Collection<NameTypeValue> getFavorites() {
        final Collection<NameTypeValue> nameTypeValues = new ArrayList<>();
        final XedNav nav = new XedNav(xed);
        try {
            final XedCursor cursorFavorites = nav.findX("/app:favorites/app:lshFavorites");
            final XedCursor cursorFavorite = nav.find("lshFavorite", cursorFavorites);
            final TypeInstance typeInstance = cursorFavorite.getTypeInstance();
            final XedCursor parentConcrete = cursorFavorite.getParentConcrete();
            final Collection<Element> children = parentConcrete.getChildren(typeInstance);
            for (final Element child : children) {
                final String context = ElementU.getAttribute(child, App.Settings.CONTEXT);
                final String name = ElementU.getAttribute(child, App.Settings.NAME);
                if (!Value.isEmpty(context)) {
                    nameTypeValues.add(new NameTypeValue(name, context, null));
                }
            }
        } catch (IOException e) {
            Logger.getLogger(getClass().getName()).severe(e.getMessage());
        }
        return nameTypeValues;
    }
}
