package io.github.greyp9.arwo.app.local.fs.menu;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;

public final class MenuLFS {

    public MenuItem toMenuItem() {
        return new MenuItem(UTF16.MENU, USER_STATE, App.Action.MENU2, "/lfs/fs", null,
                new MenuItem(App.Action.NAV_PARENT, USER_STATE, App.Action.NAV_PARENT),
                new MenuItem(App.Object.FILE, App.Target.USER_STATE, App.Action.MENU2, "/lfs/fs/file", null,
                        new MenuItem(App.Mode.CREATE_F, App.Target.USER_STATE, App.Mode.CREATE_F),
                        new MenuItem(App.Mode.CREATE_D, App.Target.USER_STATE, App.Mode.CREATE_D),
                        new MenuItem(App.Mode.EDIT, App.Target.USER_STATE, App.Mode.EDIT),
                        new MenuItem(App.Mode.RENAME_F, App.Target.USER_STATE, App.Action.TOGGLE, App.Mode.RENAME_F),
                        new MenuItem(App.Action.UPLOAD, App.Target.USER_STATE, App.Action.MENU, "/lfs/fs/file/upload"),
                        new MenuItem(App.Mode.DELETE, App.Target.USER_STATE, App.Mode.DELETE)),
                new MenuItem(App.Mode.EDIT, App.Target.USER_STATE, App.Action.MENU2, "/lfs/fs/edit", null,
                        new MenuItem("viewMime", App.Target.USER_STATE, App.Action.MENU2, "/lfs/fs/edit/viewMime", null,
                                new MenuItem("useConfig", USER_STATE, MIME_TYPE),
                                new MenuItem("viewTextPlain", USER_STATE, MIME_TYPE, Http.Mime.TEXT_PLAIN_UTF8),
                                new MenuItem("viewTextHtml", USER_STATE, MIME_TYPE, Http.Mime.TEXT_HTML_UTF8),
                                new MenuItem("viewTextXml", USER_STATE, MIME_TYPE, Http.Mime.TEXT_XML_UTF8)),
                        new MenuItem("viewCharset", USER_STATE, App.Action.MENU2, "/lfs/fs/edit/viewCharset", null,
                                new MenuItem(UTF8Codec.Const.UTF8, USER_STATE, CHARSET, UTF8Codec.Const.UTF8),
                                new MenuItem(UTF8Codec.Const.UTF16, USER_STATE, CHARSET, UTF8Codec.Const.UTF16)),
                        new MenuItem(App.Mode.FIND, App.Target.USER_STATE, App.Mode.FIND),
                        new MenuItem(App.Action.PROPERTIES, USER_STATE, App.Action.TOGGLE, App.Action.PROPERTIES),
                        new MenuItem(TEXT_EXPRESSION, USER_STATE, App.Action.TOGGLE, TEXT_EXPRESSION),
                        new MenuItem(App.Action.TEXT_FILTER, USER_STATE, App.Action.TOGGLE, App.Action.TEXT_FILTER)),
                new MenuItem(App.Mode.VIEW, App.Target.USER_STATE, App.Action.MENU2, "/lfs/fs/view", null,
                        new MenuItem(App.Mode.VIEW, null, App.Action.HREF, App.Mode.VIEW),
                        new MenuItem(App.Mode.VIEW_HEAD, null, App.Action.HREF, App.Mode.VIEW_HEAD),
                        new MenuItem(App.Mode.VIEW_TAIL, null, App.Action.HREF, App.Mode.VIEW_TAIL),
                        new MenuItem(App.Mode.VIEW_GZ, null, App.Action.HREF, App.Mode.VIEW_GZ),
                        new MenuItem(App.Mode.VIEW_ZIP, null, App.Action.HREF, App.Mode.VIEW_ZIP),
                        new MenuItem(App.Mode.VIEW_TGZ, null, App.Action.HREF, App.Mode.VIEW_TGZ),
                        new MenuItem(App.Mode.VIEW_CACHE, null, App.Action.HREF, App.Mode.VIEW_CACHE),
                        new MenuItem(App.Mode.VIEW_HEX, null, App.Action.HREF, App.Mode.VIEW_HEX),
                        new MenuItem(App.Mode.VIEW_R, null, App.Action.HREF, App.Mode.VIEW_R),
                        new MenuItem(App.Mode.VIEW_DOT, USER_STATE, App.Action.TOGGLE, App.Mode.VIEW_DOT),
                        new MenuItem(App.Action.CACHE, USER_STATE, App.Action.TOGGLE, App.Action.CACHE)),
                new MenuItem(App.Target.SESSION, App.Target.USER_STATE, App.Action.MENU2, "/lfs/fs/session", null,
                        new MenuItem(App.Action.CACHE, App.Target.USER_STATE, App.Action.TOGGLE, App.Action.CACHE),
                        new MenuItem(App.Action.CLEAR, App.Target.USER_STATE, App.Action.CLEAR),
                        new MenuItem(App.Action.REFRESH, App.Target.USER_STATE, App.Action.REFRESH),
                        new MenuItem(App.Action.CRON_ON, App.Target.USER_STATE, App.Action.CRON_ON),
                        new MenuItem(App.Action.CRON_OFF, App.Target.USER_STATE, App.Action.CRON_OFF),
                        new MenuItem(App.Action.RESET, App.Target.USER_STATE, App.Action.RESET),
                        new MenuItem(App.Action.RESTART, App.Target.USER_STATE, App.Action.RESTART),
                        new MenuItem(App.Action.STOP, App.Target.USER_STATE, App.Action.STOP)),
                new MenuItem("favorites", App.Target.USER_STATE, App.Action.MENU2, "/lfs/fs/favorites", null,
                        new MenuItem(App.Action.ADD_FAV, App.Target.SESSION, App.Action.ADD_FAV)));
    }

    private static final String CHARSET = App.Action.CHARSET;
    private static final String MIME_TYPE = App.Action.MIME_TYPE;
    private static final String TEXT_EXPRESSION = App.Action.TEXT_EXPRESSION;
    private static final String USER_STATE = App.Target.USER_STATE;
}
