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
    public final MenuItem create(final String id, final String type, final String object2) {
        MenuItem menuItem;
        final String key = Value.join(Http.Token.SLASH, id, type);
        if (Const.DASHBOARD.equals(type)) {
            menuItem = createMenuBarDash(key);
        } else if (Const.FILESYSTEM.equals(type)) {
            menuItem = createMenuBarFileSystem(key);
        } else if (Const.FILESYSTEM_STICKY.equals(type)) {
            menuItem = createMenuFilesystemSticky(key);
        } else if (Const.HEX.equals(type)) {
            menuItem = createMenuBarHex(key);
        } else if (Const.NAV.equals(type)) {
            menuItem = createMenuBarNavPages(key, object2);
        } else if (Const.COMMAND.equals(type)) {
            menuItem = createMenuBarCommand(key);
        } else if (Const.COMMAND_STICKY.equals(type)) {
            menuItem = createMenuCommandSticky(key);
        } else if (Const.VISUALIZATION.equals(type)) {
            menuItem = createMenuBarVis(key);
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
        final MenuItem itemParent = new MenuItem(App.Action.NAV_PARENT, App.Target.USER_STATE, App.Action.NAV_PARENT);
        final MenuItem[] menuItems = new MenuItem[] {
                itemParent, createMenuFile(key), createMenuEdit(key), createMenuView(key), createMenuConnection(key),
                createMenuFileSystem(key), createMenuSession(key), createMenuFavorites(key) };
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

    private static MenuItem createMenuBarNavPages(final String key, final String object2) {
        final String subject = App.Target.SESSION;
        final String action = App.Action.NAV_PARAM;
        final MenuItem itemFirst = new MenuItem(UTF16.ARROW_FIRST, subject, action, ViewState.Nav.FIRST, object2);
        final MenuItem itemPrev = new MenuItem(UTF16.ARROW_LEFT, subject, action, ViewState.Nav.PREVIOUS, object2);
        final MenuItem itemNext = new MenuItem(UTF16.ARROW_RIGHT, subject, action, ViewState.Nav.NEXT, object2);
        final MenuItem itemLast = new MenuItem(UTF16.ARROW_LAST, subject, action, ViewState.Nav.LAST, object2);
        final MenuItem menu = new MenuItem(action, subject, action, key + "/navPage", (String) null,
                itemFirst, itemPrev, itemNext, itemLast);
        menu.setOpen(true);
        return menu;
    }

    private static MenuItem createMenuBarCommand(final String key) {
        final MenuItem[] menuItems = new MenuItem[] {
                createMenuConnection(key), createMenuCommand(key), createMenuSession(key), createMenuFavorites(key) };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuBarVis(final String key) {
        final MenuItem[] menuItems = new MenuItem[] {
                createMenuViewVis(key), createMenuSession(key) };
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuFile(final String key) {
        final MenuItem itemFileNew = new MenuItem(App.Mode.CREATE_F, App.Target.USER_STATE, App.Mode.CREATE_F);
        final MenuItem itemFolderNew = new MenuItem(App.Mode.CREATE_D, App.Target.USER_STATE, App.Mode.CREATE_D);
        final MenuItem itemFileEdit = new MenuItem(App.Mode.EDIT, App.Target.USER_STATE, App.Mode.EDIT);
        final MenuItem itemFilesRename = new MenuItem(App.Mode.RENAME_F, App.Target.USER_STATE,
                App.Action.TOGGLE, App.Mode.RENAME_F);
        final MenuItem itemFileUpload = new MenuItem("upload", App.Target.USER_STATE, App.Action.MENU, key + "/upload");
        final MenuItem itemFileDelete = new MenuItem(App.Mode.DELETE, App.Target.USER_STATE, App.Mode.DELETE);
        return new MenuItem("file", App.Target.USER_STATE, App.Action.MENU, key + "/file",
                itemFolderNew, itemFilesRename, itemFileNew, itemFileEdit, itemFileUpload, itemFileDelete);
    }

    private static MenuItem createMenuEdit(final String key) {
        final String properties = App.Action.PROPERTIES;
        final String textExpr = App.Action.TEXT_EXPRESSION;
        final String textFilter = App.Action.TEXT_FILTER;
        final MenuItem itemViewFind = new MenuItem(App.Mode.FIND, App.Target.USER_STATE, App.Mode.FIND);
        final MenuItem itemProps = new MenuItem(properties, App.Target.USER_STATE, App.Action.TOGGLE, properties);
        final MenuItem itemExpr = new MenuItem(textExpr, App.Target.USER_STATE, App.Action.TOGGLE, textExpr);
        final MenuItem itemFilter = new MenuItem(textFilter, App.Target.USER_STATE, App.Action.TOGGLE, textFilter);
        return new MenuItem(App.Mode.EDIT, App.Target.USER_STATE, App.Action.MENU, Value.join("/", key, App.Mode.EDIT),
                createMenuViewMime(key), createMenuViewCharset(key), itemViewFind,
                itemProps, itemExpr, itemFilter);
    }

    private static MenuItem createMenuView(final String key) {
        // button strategy: ... new MenuItem(App.Mode.VIEW, App.Target.USER_STATE, App.Mode.VIEW)
        final MenuItem itemView = new MenuItem(App.Mode.VIEW, null, App.Action.HREF, App.Mode.VIEW);
        final MenuItem itemViewHead = new MenuItem(App.Mode.VIEW_HEAD, null, App.Action.HREF, App.Mode.VIEW_HEAD);
        final MenuItem itemViewTail = new MenuItem(App.Mode.VIEW_TAIL, null, App.Action.HREF, App.Mode.VIEW_TAIL);
        final MenuItem itemViewGZ = new MenuItem(App.Mode.VIEW_GZ, null, App.Action.HREF, App.Mode.VIEW_GZ);
        final MenuItem itemViewZIP = new MenuItem(App.Mode.VIEW_ZIP, null, App.Action.HREF, App.Mode.VIEW_ZIP);
        final MenuItem itemViewTGZ = new MenuItem(App.Mode.VIEW_TGZ, null, App.Action.HREF, App.Mode.VIEW_TGZ);
        final MenuItem itemViewCache = new MenuItem(App.Mode.VIEW_CACHE, null, App.Action.HREF, App.Mode.VIEW_CACHE);
        final MenuItem itemViewHex = new MenuItem(App.Mode.VIEW_HEX, null, App.Action.HREF, App.Mode.VIEW_HEX);
        final MenuItem itemViewR = new MenuItem(App.Mode.VIEW_R, null, App.Action.HREF, App.Mode.VIEW_R);
        final MenuItem itemViewDot = new MenuItem(
                App.Mode.VIEW_DOT, App.Target.USER_STATE, App.Action.TOGGLE, App.Mode.VIEW_DOT);
        final MenuItem itemCache = new MenuItem(
                App.Action.CACHE, App.Target.USER_STATE, App.Action.TOGGLE, App.Action.CACHE);
        return new MenuItem(App.Mode.VIEW, App.Target.USER_STATE, App.Action.MENU,
                Value.join(Http.Token.SLASH, key, App.Mode.VIEW),
                itemView, itemViewHead, itemViewTail, itemViewGZ, itemViewZIP,
                itemViewTGZ, itemViewCache, itemViewHex, itemViewR, itemViewDot, itemCache);
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

    private static MenuItem createMenuViewVis(final String key) {
        final String subject = App.Target.SESSION;
        final String action = App.Action.SELECT;
        final MenuItem itemMetricPrev = new MenuItem("navMetricPrev", subject, action, "navMetricPrev");
        final MenuItem itemMetricNext = new MenuItem("navMetricNext", subject, action, "navMetricNext");
        final MenuItem itemViewHtml = new MenuItem(App.Mode.VIEW_HTML, subject, action, App.Mode.VIEW_HTML);
        final MenuItem itemViewText = new MenuItem(App.Mode.VIEW_TEXT, subject, action, App.Mode.VIEW_TEXT);
        final MenuItem itemViewFile = new MenuItem(App.Mode.VIEW, subject, action, App.Mode.VIEW);
        final MenuItem itemViewLog15 = new MenuItem("log1.5", subject, action, "log1.5");
        final MenuItem itemViewLog2 = new MenuItem("log2", subject, action, "log2");
        final MenuItem itemViewLog3 = new MenuItem("log3", subject, action, "log3");

        return new MenuItem("viewVis", App.Target.USER_STATE, App.Action.MENU, key + "/viewVis",
                itemMetricPrev, itemMetricNext, itemViewHtml, itemViewText, itemViewFile,
                itemViewLog15, itemViewLog2, itemViewLog3);
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

    private static MenuItem createMenuFilesystemSticky(final String key) {
        return new MenuItem("favorites", App.Target.USER_STATE, App.Action.MENU, key + "/favorites");
    }

    private static MenuItem createMenuCommand(final String key) {
        final String subject = App.Target.SESSION;
        final MenuItem itemFileSystem = new MenuItem(App.Action.FILESYSTEM, subject, App.Action.FILESYSTEM);
        return new MenuItem(App.Action.COMMAND, App.Target.USER_STATE, App.Action.MENU, key + "/command",
                itemFileSystem);
    }

    private static MenuItem createMenuCommandSticky(final String key) {
        return new MenuItem("favorites", App.Target.USER_STATE, App.Action.MENU, key + "/favorites");
    }

    private static MenuItem createMenuSession(final String key) {
        final String subject = App.Target.USER_STATE;
        final MenuItem itemCache = new MenuItem(App.Action.CACHE, subject, App.Action.TOGGLE, App.Action.CACHE);
        final MenuItem itemClear = new MenuItem(App.Action.CLEAR, subject, App.Action.CLEAR);
        final MenuItem itemRefresh = new MenuItem(App.Action.REFRESH, subject, App.Action.REFRESH);
        final MenuItem itemCronOn = new MenuItem(App.Action.CRON_ON, subject, App.Action.CRON_ON);
        final MenuItem itemCronOff = new MenuItem(App.Action.CRON_OFF, subject, App.Action.CRON_OFF);
        final MenuItem itemReset = new MenuItem(App.Action.RESET, subject, App.Action.RESET);
        final MenuItem itemRestart = new MenuItem(App.Action.RESTART, subject, App.Action.RESTART);
        final MenuItem itemStop = new MenuItem(App.Action.STOP, subject, App.Action.STOP);
        return new MenuItem("session", subject, App.Action.MENU, key + "/session",
                itemCache, itemClear, itemRefresh, itemCronOn, itemCronOff, itemReset, itemRestart, itemStop);
    }

    private static MenuItem createMenuFavorites(final String key) {
        final MenuItem itemAddFavorite = new MenuItem(App.Action.ADD_FAV, App.Target.SESSION, App.Action.ADD_FAV);
        return new MenuItem(Const.FAVORITES, App.Target.USER_STATE, App.Action.MENU, key + "/favorites",
                itemAddFavorite);
    }

    public static class Const {
        public static final String DASHBOARD = "dash";
        public static final String FILESYSTEM = "fs";
        public static final String FILESYSTEM_STICKY = "fs-sticky";
        public static final String HEX = "hex";
        public static final String NAV = "nav";
        public static final String COMMAND = "cmd";
        public static final String COMMAND_STICKY = "cmd-sticky";
        public static final String VISUALIZATION = "vis";

        public static final String FAVORITES = "favorites";
        public static final String FILTERS = "filters";
    }
}
