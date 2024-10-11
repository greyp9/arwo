package io.github.greyp9.arwo.core.menu;

import io.github.greyp9.arwo.core.action.ActionItem;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class MenuItem extends ActionItem {
    private final Collection<MenuItem> menuItems;

    private boolean open;

    public final Collection<MenuItem> getMenuItems() {
        return menuItems;
    }

    public final MenuItem getMenuItem(final String name) {
        MenuItem menuItem = null;
        for (final MenuItem menuItemIt : menuItems) {
            if (menuItemIt.getName().equals(name)) {
                menuItem = menuItemIt;
                break;
            }
        }
        return menuItem;
    }

    public final boolean isOpen() {
        return open;
    }

    public final void setOpen(final boolean open) {
        this.open = open;
    }

    public MenuItem(final String name, final String subject,
                    final String action, final MenuItem... menuItems) {
        this(name, subject, action, "", menuItems);
    }

    public MenuItem(final String name, final String subject,
                    final String action, final String object, final MenuItem... menuItems) {
        super(name, subject, action, object, null);
        this.menuItems = new ArrayList<MenuItem>();
        Collections.addAll(this.menuItems, menuItems);
        this.open = false;
    }

    public MenuItem(final String name, final String subject,
                    final String action, final String object, final String object2, final MenuItem... menuItems) {
        super(name, subject, action, object, object2);
        this.menuItems = new ArrayList<MenuItem>();
        Collections.addAll(this.menuItems, menuItems);
        this.open = false;
    }
}
