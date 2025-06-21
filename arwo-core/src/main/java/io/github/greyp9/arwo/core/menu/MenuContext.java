package io.github.greyp9.arwo.core.menu;

import java.util.List;

public class MenuContext {
    private final MenuSystem menuSystem;
    private final List<MenuItem> menuItems;

    public MenuContext(final MenuSystem menuSystem, final List<MenuItem> menuItems) {
        this.menuSystem = menuSystem;
        this.menuItems = menuItems;
    }

    public final MenuSystem getMenuSystem() {
        return menuSystem;
    }

    public final List<MenuItem> getMenuItems() {
        return menuItems;
    }
}
