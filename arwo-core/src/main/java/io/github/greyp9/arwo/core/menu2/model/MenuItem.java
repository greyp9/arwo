package io.github.greyp9.arwo.core.menu2.model;

import io.github.greyp9.arwo.core.action.ActionItem;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

public class MenuItem extends ActionItem {
    private final List<MenuItem> menuItems;

    public final Collection<MenuItem> getMenuItems() {
        return menuItems;
    }

    private boolean open;

    public final boolean isOpen() {
        return open;
    }

    public final void setOpen(final boolean open) {
        this.open = open;
    }

    public MenuItem(final String name,
                    final String subject,
                    final String action) {
        this(name, subject, action, "", "");
    }

    public MenuItem(final String name,
                    final String subject,
                    final String action,
                    final String object) {
        this(name, subject, action, object, "");
    }

    public MenuItem(final String name,
                    final String subject,
                    final String action,
                    final String object,
                    final String object2,
                    final MenuItem... menuItems) {
        super(name, subject, action, object, object2);
        this.menuItems = new ArrayList<>();
        Collections.addAll(this.menuItems, menuItems);
        this.open = false;
    }

    public MenuItem(final String name,
                    final String subject,
                    final String action,
                    final String object,
                    final String object2,
                    final List<MenuItem> menuItems) {
        super(name, subject, action, object, object2);
        this.menuItems = new ArrayList<>(menuItems);
        this.open = false;
    }

    public final MenuItem applyFrom(final Properties menuSystemState) {
        new MenuState(menuSystemState).applyTo(this);
        return this;
    }
}
