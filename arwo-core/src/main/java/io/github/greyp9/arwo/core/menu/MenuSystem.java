package io.github.greyp9.arwo.core.menu;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;
import io.github.greyp9.arwo.core.value.Value;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

public class MenuSystem {
    private final Map<String, MenuItem> menus;
    private final String submitID;
    private final MenuFactory factory;

    public final String getSubmitID() {
        return submitID;
    }

    public MenuSystem(final String submitID, final MenuFactory factory) {
        this.menus = new TreeMap<String, MenuItem>();
        this.submitID = submitID;
        this.factory = factory;
    }

    public final void applyState(final Properties menuSystemState) {
        for (Map.Entry<Object, Object> entry : menuSystemState.entrySet()) {
            if (Boolean.parseBoolean(entry.getValue().toString())) {
                toggle(entry.getKey().toString());
            }
        }
    }

    public final MenuItem get(final String id, final String type) {
        return get(id, type, null);
    }

    public final MenuItem get(final String id, final String type, final String object2) {
        final String key = Value.join(Http.Token.SLASH, id, type, object2);
        //menus.clear();  // menu dev hook, always create menus
        if (!menus.containsKey(key)) {
            menus.put(key, factory.create(id, type, object2));
        }
        return menus.get(key);
    }

    public final boolean isOpen(final String object) {
        boolean isOpen = false;
        for (final MenuItem menuItem : menus.values()) {
            isOpen |= isOpen(Collections.singleton(menuItem), object);
        }
        return isOpen;
    }

    private boolean isOpen(final Collection<MenuItem> menuItems, final String object) {
        boolean isOpen = false;
        for (final MenuItem menuItem : menuItems) {
            if (menuItem.getObject().equals(object)) {
                isOpen |= menuItem.isOpen();
            } else {
                isOpen |= isOpen(menuItem.getMenuItems(), object);
            }
        }
        return isOpen;
    }

    public final void toggle(final String object) {
        for (final MenuItem menuItem : menus.values()) {
            toggle(Collections.singleton(menuItem), object);
        }
    }

    private void toggle(final Collection<MenuItem> menuItems, final String object) {
        for (final MenuItem menuItem : menuItems) {
            if (menuItem.getObject().equals(object)) {
                toggle(menuItems, menuItem);
            }
            toggle(menuItem.getMenuItems(), object);
        }
    }

    private void toggle(final Collection<MenuItem> menuItems, final MenuItem menuItem) {
        final boolean open = menuItem.isOpen();
        // close all menu items
        for (final MenuItem menuItemIt : menuItems) {
            menuItemIt.setOpen(false);
        }
        // open selected menu item
        menuItem.setOpen(!open);
    }
}
