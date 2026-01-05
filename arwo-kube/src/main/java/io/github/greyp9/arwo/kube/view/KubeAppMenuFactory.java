package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;
import io.github.greyp9.arwo.core.value.Value;

@Deprecated  // 2025-12T
public final class KubeAppMenuFactory implements MenuFactory {
    public static final String KUBE_LOG_OPTIONS = "kube-log-options";

    @Override
    public MenuItem create(final String id, final String type, final String object2) {
        // Edit
        final MenuItem itemExpression = new MenuItem(
                App.Action.TEXT_EXPRESSION, App.Target.USER_STATE, App.Action.TOGGLE, App.Action.TEXT_EXPRESSION);
        final MenuItem menuEdit = new MenuItem(App.Mode.EDIT, App.Target.USER_STATE, App.Action.MENU,
                Value.join("/", KUBE, App.Mode.EDIT), itemExpression);
        // Session
        final MenuItem itemClear = new MenuItem(
                App.Action.CLEAR, App.Target.USER_STATE, App.Action.CLEAR);
        final MenuItem menuSession = new MenuItem(
                "session", App.Target.USER_STATE, App.Action.MENU, KUBE + "/session", itemClear);
        // Log Options
        final MenuItem itemOptions = new MenuItem(
                KUBE_LOG_OPTIONS, App.Target.USER_STATE, App.Action.TOGGLE, KUBE_LOG_OPTIONS);
        // kube
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, menuEdit, menuSession, itemOptions);
    }

    public static final String KUBE = "kube";
}
