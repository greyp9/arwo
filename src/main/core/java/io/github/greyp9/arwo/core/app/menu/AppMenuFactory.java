package io.github.greyp9.arwo.core.app.menu;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.Value;

// i18nf
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
        final MenuItem[] menuItems = new MenuItem[] {
                createMenuFile(key), createMenuView(key), createMenuConnection(key), createMenuFileSystem(key),
                createMenuSession(key), createMenuFavorites(key) };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuBarHex(final String key) {
        final String subject = App.Target.USER_STATE;
        final String action = App.Action.HEX_VIEW_PARAM;
        final MenuItem itemFirst = new MenuItem(UTF16.ARROW_FIRST, subject, action, ViewState.Nav.FIRST);
        final MenuItem itemPrev = new MenuItem(UTF16.ARROW_LEFT, subject, action, ViewState.Nav.PREVIOUS);
        final MenuItem itemNext = new MenuItem(UTF16.ARROW_RIGHT, subject, action, ViewState.Nav.NEXT);
        final MenuItem itemLast = new MenuItem(UTF16.ARROW_LAST, subject, action, ViewState.Nav.LAST);
        final MenuItem item16 = new MenuItem(App.Hex.WIDTH_16, subject, action, App.Hex.WIDTH_16);
        final MenuItem item32 = new MenuItem(App.Hex.WIDTH_32, subject, action, App.Hex.WIDTH_32);
        final MenuItem item64 = new MenuItem(App.Hex.WIDTH_64, subject, action, App.Hex.WIDTH_64);
        final MenuItem menuHex = new MenuItem(action, subject, action, key + "/viewHex",
                itemFirst, itemPrev, itemNext, itemLast, item16, item32, item64);
        menuHex.setOpen(true);
        return menuHex;
    }

    private static MenuItem createMenuBarCommand(final String key) {
        final MenuItem[] menuItems = new MenuItem[] {
                createMenuConnection(key), createMenuCommand(key), createMenuSession(key), createMenuFavorites(key) };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuFile(final String key) {
        final MenuItem itemFileNew = new MenuItem(App.Mode.CREATE_F, App.Target.USER_STATE, App.Mode.CREATE_F);
        final MenuItem itemFolderNew = new MenuItem(App.Mode.CREATE_D, App.Target.USER_STATE, App.Mode.CREATE_D);
        final MenuItem itemFileEdit = new MenuItem(App.Mode.EDIT, App.Target.USER_STATE, App.Mode.EDIT);
        final MenuItem itemFileUpload = new MenuItem("upload", App.Target.USER_STATE, App.Action.MENU, key + "/upload");
        final MenuItem itemFileDelete = new MenuItem(App.Mode.DELETE, App.Target.USER_STATE, App.Mode.DELETE);
        return new MenuItem("file", App.Target.USER_STATE, App.Action.MENU, key + "/file",
                itemFileNew, itemFolderNew, itemFileEdit, itemFileUpload, itemFileDelete);
    }

    private static MenuItem createMenuView(final String key) {
        final String properties = App.Action.PROPERTIES;
        final String textExpr = App.Action.TEXT_EXPRESSION;
        final String textFilter = App.Action.TEXT_FILTER;
        //final MenuItem itemViewFind = new MenuItem(find, App.Target.USER_STATE, App.Action.TOGGLE, App.Action.FIND);
        final MenuItem itemView = new MenuItem(App.Mode.VIEW, App.Target.USER_STATE, App.Mode.VIEW);
        final MenuItem itemViewGZ = new MenuItem(App.Mode.VIEW_GZ, App.Target.USER_STATE, App.Mode.VIEW_GZ);
        final MenuItem itemViewZIP = new MenuItem(App.Mode.VIEW_ZIP, App.Target.USER_STATE, App.Mode.VIEW_ZIP);
        final MenuItem itemViewTGZ = new MenuItem(App.Mode.VIEW_TGZ, App.Target.USER_STATE, App.Mode.VIEW_TGZ);
        final MenuItem itemViewHex = new MenuItem(App.Mode.VIEW_HEX, App.Target.USER_STATE, App.Mode.VIEW_HEX);
        final MenuItem itemViewR = new MenuItem(App.Mode.VIEW_R, App.Target.USER_STATE, App.Mode.VIEW_R);
        final MenuItem itemProps = new MenuItem(properties, App.Target.USER_STATE, App.Action.TOGGLE, properties);
        final MenuItem itemExpr = new MenuItem(textExpr, App.Target.USER_STATE, App.Action.TOGGLE, textExpr);
        final MenuItem itemFilter = new MenuItem(textFilter, App.Target.USER_STATE, App.Action.TOGGLE, textFilter);
        return new MenuItem(App.Mode.VIEW, App.Target.USER_STATE, App.Action.MENU, Value.join("/", key, App.Mode.VIEW),
                createMenuViewMime(key), createMenuViewCharset(key), /*itemViewFind,*/
                itemView, itemViewGZ, itemViewZIP, itemViewTGZ, itemViewHex, itemViewR,
                itemProps, itemExpr, itemFilter);
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

    private static MenuItem createMenuConnection(final String key) {
        final String properties = App.Action.PROPERTIES;
        final MenuItem itemProps = new MenuItem(properties, App.Target.SESSION, App.Action.TOGGLE, properties);
        return new MenuItem("connection", App.Target.USER_STATE, App.Action.MENU, key + "/connection",
                itemProps);
    }

    private static MenuItem createMenuFileSystem(final String key) {
        final String subject = App.Target.SESSION;
        final MenuItem itemCommand = new MenuItem(App.Action.COMMAND, subject, App.Action.COMMAND);
        return new MenuItem(App.Action.FILESYSTEM, App.Target.USER_STATE, App.Action.MENU, key + "/filesystem",
                itemCommand);
    }

    private static MenuItem createMenuCommand(final String key) {
        final String subject = App.Target.SESSION;
        final MenuItem itemFileSystem = new MenuItem(App.Action.FILESYSTEM, subject, App.Action.FILESYSTEM);
        return new MenuItem(App.Action.COMMAND, App.Target.USER_STATE, App.Action.MENU, key + "/command",
                itemFileSystem);
    }

    private static MenuItem createMenuSession(final String key) {
        final String subject = App.Target.USER_STATE;
        final MenuItem itemClear = new MenuItem(App.Action.CLEAR, subject, App.Action.CLEAR);
        final MenuItem itemCronOn = new MenuItem(App.Action.CRON_ON, subject, App.Action.CRON_ON);
        final MenuItem itemCronOff = new MenuItem(App.Action.CRON_OFF, subject, App.Action.CRON_OFF);
        final MenuItem itemReset = new MenuItem(App.Action.RESET, subject, App.Action.RESET);
        final MenuItem itemRestart = new MenuItem(App.Action.RESTART, subject, App.Action.RESTART);
        final MenuItem itemStop = new MenuItem(App.Action.STOP, subject, App.Action.STOP);
        return new MenuItem("session", subject, App.Action.MENU, key + "/session",
                itemClear, itemCronOn, itemCronOff, itemReset, itemRestart, itemStop);
    }

    private static MenuItem createMenuFavorites(final String key) {
        final MenuItem itemAddFavorite = new MenuItem(App.Action.ADD_FAV, App.Target.SESSION, App.Action.ADD_FAV);
        return new MenuItem(Const.FAVORITES, App.Target.USER_STATE, App.Action.MENU, key + "/favorites",
                itemAddFavorite);
    }

    public static class Const {
        public static final String DASHBOARD = "dash";
        public static final String FILESYSTEM = "fs";
        public static final String HEX = "hex";
        public static final String COMMAND = "cmd";

        public static final String FAVORITES = "favorites";
    }
}
