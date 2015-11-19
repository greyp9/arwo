package io.github.greyp9.arwo.core.xed.menu;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;
import io.github.greyp9.arwo.core.value.Value;

public class XedMenuFactory implements MenuFactory {

    public final MenuItem create(final String id, final String type) {
        MenuItem menuItem;
        final String key = Value.join(Http.Token.SLASH, id, type);
        if (Const.XED.equals(type)) {
            menuItem = createMenuBarXed(key);
        } else {
            menuItem = new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU);
        }
        return menuItem;
    }

    private static MenuItem createMenuBarXed(final String key) {
        final MenuItem[] menuItems = new MenuItem[]{
                createMenuDocument(key), createMenuClipboard(key), createMenuView(key)};
        return new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU, key, menuItems);
    }

    private static MenuItem createMenuClipboard(final String key) {
        final MenuItem itemCut = new MenuItem(App.Action.CLIP_CUT, App.Target.DOCUMENT, App.Action.CLIP_CUT);
        final MenuItem itemCopy = new MenuItem(App.Action.CLIP_COPY, App.Target.DOCUMENT, App.Action.CLIP_COPY);
        final MenuItem itemPaste = new MenuItem(App.Action.CLIP_PASTE, App.Target.DOCUMENT, App.Action.CLIP_PASTE);
        final MenuItem itemClear = new MenuItem(App.Action.CLIP_CLEAR, App.Target.DOCUMENT, App.Action.CLIP_CLEAR);
        return new MenuItem("clipboard", App.Target.USER_STATE, App.Action.MENU,
                key + "/clipboard", itemCut, itemCopy, itemPaste, itemClear);
    }

    private static MenuItem createMenuDocument(final String key) {
        // schema validate document
        // fill in missing document content
        // pretty print document
        // reload document from filesystem
        // save document (fs & zip revision)
        final MenuItem itemValidate = new MenuItem(App.Action.VALIDATE, App.Target.SESSION, App.Action.VALIDATE);
        final MenuItem itemFill = new MenuItem(App.Action.FILL, App.Target.DOCUMENT, App.Action.FILL);
        final MenuItem itemPrune = new MenuItem(App.Action.PRUNE, App.Target.DOCUMENT, App.Action.PRUNE);
        final MenuItem itemPretty = new MenuItem(App.Action.PRETTY, App.Target.SESSION, App.Action.PRETTY);
        final MenuItem itemReload = new MenuItem(App.Action.RELOAD, App.Target.SESSION, App.Action.RELOAD);
        final MenuItem itemSave = new MenuItem(App.Action.SAVE, App.Target.SESSION, App.Action.SAVE);
        final MenuItem itemCommit = new MenuItem(App.Action.COMMIT, App.Target.USER_STATE, App.Action.COMMIT);
        return new MenuItem("document", App.Target.USER_STATE, App.Action.MENU,
                key + "/document", itemValidate, itemFill, itemPrune, itemPretty, itemReload, itemSave, itemCommit);
    }

    private static MenuItem createMenuView(final String key) {
        // view document xml (at cursor)
        // view document schema
        // view document xsd types
        // view document revisions
        final MenuItem itemUI = new MenuItem("ui", App.Target.USER_STATE, "ui");
        final MenuItem itemXml = new MenuItem("xml", App.Target.USER_STATE, "xml");
        final MenuItem itemXsd = new MenuItem("xsd", App.Target.USER_STATE, "xsd");
        final MenuItem itemRev = new MenuItem("rev", App.Target.USER_STATE, "rev");
        final MenuItem itemLocale = new MenuItem("locale", App.Target.USER_STATE, "locale");
        return new MenuItem("view", App.Target.USER_STATE, App.Action.MENU,
                key + "/view", itemUI, itemXml, itemXsd, itemRev, itemLocale);
    }

    public static class Const {
        public static final String XED = "xed";
    }
}
