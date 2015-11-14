package io.github.greyp9.arwo.core.menu.factory;

import io.github.greyp9.arwo.core.menu.MenuItem;

public interface MenuFactory {
    MenuItem create(String id, String type);
}
