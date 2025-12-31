package io.github.greyp9.arwo.core.menu2.model;

import java.util.Collection;
import java.util.Properties;

public final class MenuState {
    private final Properties menuState;

    public MenuState(final Properties menuState) {
        this.menuState = menuState;
    }

    public void applyTo(final Collection<MenuItem> menuItems) {
        for (MenuItem menuItem : menuItems) {
            applyTo(menuItem);
        }
    }

    public void applyTo(final MenuItem... menuItems) {
        for (MenuItem menuItem : menuItems) {
            applyTo(menuItem);
        }
    }

    public void applyTo(final MenuItem menuItem) {
        if (Boolean.parseBoolean(menuState.getProperty(menuItem.getObject()))) {
            menuItem.setOpen(true);
        }
        for (MenuItem menuItem1 : menuItem.getMenuItems()) {
            applyTo(menuItem1);
        }
    }
}
