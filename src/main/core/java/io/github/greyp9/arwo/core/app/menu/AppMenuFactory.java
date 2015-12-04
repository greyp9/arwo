package io.github.greyp9.arwo.core.app.menu;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;
import io.github.greyp9.arwo.core.value.Value;

public class AppMenuFactory implements MenuFactory {

    @Override
    public final MenuItem create(final String id, final String type) {
        MenuItem menuItem;
        final String key = Value.join(Http.Token.SLASH, id, type);
        if (Const.FILESYSTEM.equals(type)) {
            menuItem = createMenuBarFileSystem(key);
        } else {
            menuItem = new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU);
        }
        return menuItem;
    }

    private static MenuItem createMenuBarFileSystem(final String key) {
        final MenuItem[] menuItems = new MenuItem[] { createMenuView(key) };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuView(final String key) {
        final MenuItem itemViewUtf8 = new MenuItem("viewUTF8", App.Target.USER_STATE, "view");
        final MenuItem itemViewUtf16 = new MenuItem("viewUTF16", App.Target.USER_STATE, "view16");
        final MenuItem itemViewGZ = new MenuItem("viewGZ", App.Target.USER_STATE, "viewGZ");
        final MenuItem itemViewZIP = new MenuItem("viewZIP", App.Target.USER_STATE, "viewZIP");
        final MenuItem itemViewTGZ = new MenuItem("viewTGZ", App.Target.USER_STATE, "viewTGZ");
        final MenuItem itemViewHex = new MenuItem("viewHex", App.Target.USER_STATE, "viewHex");
        final MenuItem itemEditUtf8 = new MenuItem("editUTF8", App.Target.USER_STATE, "edit");
        final MenuItem itemEditUtf16 = new MenuItem("editUTF16", App.Target.USER_STATE, "edit16");
        final MenuItem itemProperties = new MenuItem("properties", App.Target.USER_STATE, "properties");
        final MenuItem itemTextFilters = new MenuItem("textFilters", App.Target.USER_STATE, "textFilters");
        return new MenuItem("view", App.Target.USER_STATE, App.Action.MENU, key + "/view",
                createMenuViewMime(key),
                itemViewUtf8, itemViewUtf16, itemViewGZ, itemViewZIP, itemViewTGZ, itemViewHex,
                itemEditUtf8, itemEditUtf16, itemProperties, itemTextFilters);
    }

    private static MenuItem createMenuViewMime(final String key) {
        final MenuItem itemUseConfig = new MenuItem("useConfig", App.Target.USER_STATE, "viewUseConfig");
        final MenuItem itemTextPlain = new MenuItem("viewTextPlain", App.Target.USER_STATE, "viewTextPlain");
        final MenuItem itemTextHtml = new MenuItem("viewTextHtml", App.Target.USER_STATE, "viewTextHtml");
        final MenuItem itemTextXml = new MenuItem("viewTextXml", App.Target.USER_STATE, "viewTextXml");
        return new MenuItem("viewMime", App.Target.USER_STATE, App.Action.MENU, key + "/viewMime",
                itemUseConfig, itemTextPlain, itemTextHtml, itemTextXml);
    }

    public static class Const {
        public static final String FILESYSTEM = "fs";
    }
}
