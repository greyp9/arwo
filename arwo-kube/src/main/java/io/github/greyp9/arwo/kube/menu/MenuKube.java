package io.github.greyp9.arwo.kube.menu;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.menu2.core.MenuSession;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.resource.PathU;

public final class MenuKube {

    public MenuItem toMenuItem() {
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU2, MENU_KEY, null,
                new MenuItem(App.Mode.EDIT, App.Target.USER_STATE, App.Action.MENU2, MENU_EDIT, null,
                        new MenuItem(TEXT_EXPRESSION, App.Target.USER_STATE, App.Action.TOGGLE, TEXT_EXPRESSION)),
                new MenuSession().toMenuItem(MENU_SESSION),
                new MenuItem(KUBE_LOG_OPTIONS, App.Target.USER_STATE, App.Action.MENU2, MENU_LOG_OPTIONS));
    }

    private static final String KUBE_LOG_OPTIONS = "kube-log-options";

    private static final String MENU_KEY = "/menu2/kube";
    private static final String MENU_EDIT = PathU.toPath(MENU_KEY, App.Mode.EDIT);
    private static final String MENU_SESSION = PathU.toPath(MENU_KEY, App.Target.SESSION);
    public static final String MENU_LOG_OPTIONS = PathU.toPath(MENU_KEY, KUBE_LOG_OPTIONS);

    private static final String TEXT_EXPRESSION = App.Action.TEXT_EXPRESSION;
}
