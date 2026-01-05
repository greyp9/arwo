package io.github.greyp9.arwo.app.vis.menu;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.menu2.core.MenuSession;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.resource.PathU;

public final class MenuVis {

    public MenuItem toMenuItem() {
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU2, MENU_KEY, null,
                new MenuItem("viewVis", App.Target.USER_STATE, App.Action.MENU2, MENU_VIEW, null,
                        new MenuItem("navMetricPrev", SESSION, SELECT, "navMetricPrev"),
                        new MenuItem("navMetricNext", SESSION, SELECT, "navMetricNext"),
                        new MenuItem(App.Mode.VIEW_HTML, SESSION, SELECT, App.Mode.VIEW_HTML),
                        new MenuItem(App.Mode.VIEW_TEXT, SESSION, SELECT, App.Mode.VIEW_TEXT),
                        new MenuItem(App.Mode.VIEW, SESSION, SELECT, App.Mode.VIEW),
                        new MenuItem("log1.5", SESSION, SELECT, "log1.5"),
                        new MenuItem("log2", SESSION, SELECT, "log2"),
                        new MenuItem("log3", SESSION, SELECT, "log3")),
                new MenuSession().toMenuItem(MENU_SESSION));
    }

    private static final String MENU_KEY = "/menu2/vis/main/1";
    private static final String MENU_VIEW = PathU.toPath(MENU_KEY, "viewVis");
    private static final String MENU_SESSION = PathU.toPath(MENU_KEY, App.Target.SESSION);

    private static final String SELECT = App.Action.SELECT;
    private static final String SESSION = App.Target.SESSION;
}
