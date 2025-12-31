package io.github.greyp9.arwo.core.menu2.core;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;

public final class MenuSession {

    public MenuItem toMenuItem(final String key) {
        return new MenuItem(App.Target.SESSION, App.Target.USER_STATE, App.Action.MENU2, key, null,
                new MenuItem(App.Action.CACHE, App.Target.USER_STATE, App.Action.TOGGLE, App.Action.CACHE),
                new MenuItem(App.Action.CLEAR, App.Target.USER_STATE, App.Action.CLEAR),
                new MenuItem(App.Action.REFRESH, App.Target.USER_STATE, App.Action.REFRESH),
                new MenuItem(App.Action.CRON_ON, App.Target.USER_STATE, App.Action.CRON_ON),
                new MenuItem(App.Action.CRON_OFF, App.Target.USER_STATE, App.Action.CRON_OFF),
                new MenuItem(App.Action.RESET, App.Target.USER_STATE, App.Action.RESET),
                new MenuItem(App.Action.RESTART, App.Target.USER_STATE, App.Action.RESTART),
                new MenuItem(App.Action.STOP, App.Target.USER_STATE, App.Action.STOP));
    }
}
