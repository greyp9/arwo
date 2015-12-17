package io.github.greyp9.arwo.core.app.menu;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;
import io.github.greyp9.arwo.core.value.Value;

@SuppressWarnings("PMD.TooManyMethods")
public class AppMenuFactory implements MenuFactory {

    @Override
    public final MenuItem create(final String id, final String type) {
        MenuItem menuItem;
        final String key = Value.join(Http.Token.SLASH, id, type);
        if (Const.DASHBOARD.equals(type)) {
            menuItem = createMenuBarDash(key);
        } else if (Const.FILESYSTEM.equals(type)) {
            menuItem = createMenuBarFileSystem(key);
        } else if (Const.HEX.equals(type)) {
            menuItem = createMenuBarHex(key);
        } else if (Const.COMMAND.equals(type)) {
            menuItem = createMenuBarCommand(key);
        } else {
            menuItem = new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU);
        }
        return menuItem;
    }

    private static MenuItem createMenuBarDash(final String key) {
        final MenuItem[] menuItems = new MenuItem[] {
                createMenuSession(key) };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuBarFileSystem(final String key) {
        final MenuItem[] menuItems = new MenuItem[] { createMenuFile(key), createMenuView(key),
                createMenuFileSystem(key), createMenuSession(key), createMenuFavorites(key) };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuBarHex(final String key) {
        final String subject = App.Target.USER_STATE;
        final String action = App.Action.HEX_VIEW_PARAM;
        final MenuItem itemFirst = new MenuItem(widen(UTF16.ARROW_FIRST), subject, action, "first");
        final MenuItem itemPrev = new MenuItem(widen(UTF16.ARROW_LEFT), subject, action, "prev");
        final MenuItem itemNext = new MenuItem(widen(UTF16.ARROW_RIGHT), subject, action, "next");
        final MenuItem itemLast = new MenuItem(widen(UTF16.ARROW_LAST), subject, action, "last");
        final MenuItem item16 = new MenuItem(widen("16"), subject, action, "16");
        final MenuItem item32 = new MenuItem(widen("32"), subject, action, "32");
        final MenuItem item64 = new MenuItem(widen("64"), subject, action, "64");
        final MenuItem menuHex = new MenuItem(action, subject, action, key + "/viewHex",
                itemFirst, itemPrev, itemNext, itemLast, item16, item32, item64);
        menuHex.setOpen(true);
        return menuHex;
    }

    private static MenuItem createMenuBarCommand(final String key) {
        final MenuItem[] menuItems = new MenuItem[] {
                createMenuCommand(key), createMenuSession(key), createMenuFavorites(key) };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuFile(final String key) {
        final MenuItem itemFileNew = new MenuItem("create", App.Target.USER_STATE, "create");
        final MenuItem itemFileEdit = new MenuItem("edit", App.Target.USER_STATE, "edit");
        final MenuItem itemFileUpload = new MenuItem("upload", App.Target.USER_STATE, App.Action.MENU, key + "/upload");
        return new MenuItem("file", App.Target.USER_STATE, App.Action.MENU, key + "/file",
                itemFileNew, itemFileEdit, itemFileUpload);
    }

    private static MenuItem createMenuView(final String key) {
        final String find = App.Action.FIND;
        final String properties = App.Action.PROPERTIES;
        final String textFilter = App.Action.TEXT_FILTER;
        final MenuItem itemViewFind = new MenuItem(find, App.Target.USER_STATE, App.Action.TOGGLE, find);
        final MenuItem itemView = new MenuItem("view", App.Target.USER_STATE, "view");
        final MenuItem itemViewGZ = new MenuItem("viewGZ", App.Target.USER_STATE, "viewGZ");
        final MenuItem itemViewZIP = new MenuItem("viewZIP", App.Target.USER_STATE, "viewZIP");
        final MenuItem itemViewTGZ = new MenuItem("viewTGZ", App.Target.USER_STATE, "viewTGZ");
        final MenuItem itemViewHex = new MenuItem("viewHex", App.Target.USER_STATE, "viewHex");
        final MenuItem itemProps = new MenuItem(properties, App.Target.USER_STATE, App.Action.TOGGLE, properties);
        final MenuItem itemFilter = new MenuItem(textFilter, App.Target.USER_STATE, App.Action.TOGGLE, textFilter);
        return new MenuItem("view", App.Target.USER_STATE, App.Action.MENU, key + "/view",
                createMenuViewMime(key), createMenuViewCharset(key), itemViewFind,
                itemView, itemViewGZ, itemViewZIP, itemViewTGZ, itemViewHex, itemProps, itemFilter);
    }

    private static MenuItem createMenuViewMime(final String key) {
        final String subject = App.Target.USER_STATE;
        final String action = App.Action.MIME_TYPE;
        final MenuItem itemUseConfig = new MenuItem("useConfig", subject, action);
        final MenuItem itemTextPlain = new MenuItem("viewTextPlain", subject, action, Http.Mime.TEXT_PLAIN_UTF8);
        final MenuItem itemTextHtml = new MenuItem("viewTextHtml", subject, action, Http.Mime.TEXT_HTML_UTF8);
        final MenuItem itemTextXml = new MenuItem("viewTextXml", subject, action, Http.Mime.TEXT_XML_UTF8);
        return new MenuItem("viewMime", subject, App.Action.MENU, key + "/viewMime",
                itemUseConfig, itemTextPlain, itemTextHtml, itemTextXml);
    }

    private static MenuItem createMenuViewCharset(final String key) {
        final String subject = App.Target.USER_STATE;
        final String action = App.Action.CHARSET;
        final MenuItem itemUTF8 = new MenuItem(UTF8Codec.Const.UTF8, subject, action, UTF8Codec.Const.UTF8);
        final MenuItem itemUTF16 = new MenuItem(UTF8Codec.Const.UTF16, subject, action, UTF8Codec.Const.UTF16);
        return new MenuItem("viewCharset", subject, App.Action.MENU, key + "/viewCharset",
                itemUTF8, itemUTF16);
    }

    private static MenuItem createMenuFileSystem(final String key) {
        final String subject = App.Target.SESSION;
        final MenuItem itemCommand = new MenuItem(App.Action.COMMAND, subject, App.Action.COMMAND);
        return new MenuItem("filesystem", App.Target.USER_STATE, App.Action.MENU, key + "/filesystem",
                itemCommand);
    }

    private static MenuItem createMenuCommand(final String key) {
        final String subject = App.Target.SESSION;
        final MenuItem itemFileSystem = new MenuItem(App.Action.FILESYSTEM, subject, App.Action.FILESYSTEM);
        return new MenuItem("command", App.Target.USER_STATE, App.Action.MENU, key + "/command",
                itemFileSystem);
    }

    private static MenuItem createMenuSession(final String key) {
        final String subject = App.Target.USER_STATE;
        final MenuItem itemClear = new MenuItem(App.Action.CLEAR, subject, App.Action.CLEAR);
        final MenuItem itemReset = new MenuItem(App.Action.RESET, subject, App.Action.RESET);
        return new MenuItem("session", subject, App.Action.MENU, key + "/session",
                itemClear, itemReset);
    }

    private static MenuItem createMenuFavorites(final String key) {
        final MenuItem itemAddFavorite = new MenuItem(App.Action.ADD_FAV, App.Target.SESSION, App.Action.ADD_FAV);
        return new MenuItem("favorites", App.Target.USER_STATE, App.Action.MENU, key + "/favorites",
                itemAddFavorite);
    }

    private static String widen(final String text) {  // bigger menu target
        return String.format("[ %s ]", text);
    }

    public static class Const {
        public static final String DASHBOARD = "dash";
        public static final String FILESYSTEM = "fs";
        public static final String HEX = "hex";
        public static final String COMMAND = "cmd";
    }
}
