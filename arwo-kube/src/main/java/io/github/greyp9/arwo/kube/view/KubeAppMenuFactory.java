package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;

public final class KubeAppMenuFactory implements MenuFactory {

    @Override
    public MenuItem create(final String id, final String type, final String object2) {
        final String subject = App.Target.USER_STATE;
        final MenuItem[] menuItems = new MenuItem[] {
                new MenuItem(App.Action.CLEAR, subject, App.Action.CLEAR),
        };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, KUBE, menuItems);
    }

    public static final String KUBE = "kube";
}
