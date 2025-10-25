package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;

public final class KubeAppMenuFactory implements MenuFactory {
    public static final String KUBE_LOG_OPTIONS = "kube-log-options";

    @Override
    public MenuItem create(final String id, final String type, final String object2) {
        final String subject = App.Target.USER_STATE;
        final MenuItem itemClear = new MenuItem(App.Action.CLEAR, subject, App.Action.CLEAR);
        final MenuItem itemOptions = new MenuItem(KUBE_LOG_OPTIONS, subject, App.Action.TOGGLE, KUBE_LOG_OPTIONS);
        final MenuItem menuItemsSession = new MenuItem(
                "session", subject, App.Action.MENU, KUBE + "/session", itemClear);
        return new MenuItem(UTF16.MENU, subject, App.Action.MENU, menuItemsSession, itemOptions);
    }

    public static final String KUBE = "kube";
}
