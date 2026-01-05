package io.github.greyp9.arwo.app.core.view.hex;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.value.Value;

public final class MenuHex {

    public MenuItem toMenuItem() {
        final String subject = App.Target.USER_STATE;
        final String action = App.Action.HEX_VIEW_PARAM;
        final MenuItem menuItem = new MenuItem("View (Hex)", App.Target.USER_STATE, App.Action.MENU2, MENU_KEY, null,
                new MenuItem(getLabel(UTF16.ARROW_FIRST), subject, action, ViewState.Nav.FIRST),
                new MenuItem(getLabel(UTF16.ARROW_LEFT), subject, action, ViewState.Nav.PREVIOUS),
                new MenuItem(getLabel(UTF16.ARROW_RIGHT), subject, action, ViewState.Nav.NEXT),
                new MenuItem(getLabel(UTF16.ARROW_LAST), subject, action, ViewState.Nav.LAST),
                new MenuItem(getLabel(App.Hex.WIDTH_16), subject, action, App.Hex.WIDTH_16),
                new MenuItem(getLabel(App.Hex.WIDTH_32), subject, action, App.Hex.WIDTH_32),
                new MenuItem(getLabel(App.Hex.WIDTH_64), subject, action, App.Hex.WIDTH_64));
        menuItem.setOpen(true);
        return menuItem;
    }

    private String getLabel(final String name) {
        return Value.wrap(App.Token.BRACKET_OPEN, App.Token.BRACKET_CLOSE, Value.wrap(Html.SPACE, name));
    }

    private static final String MENU_KEY = "/menu2/viewHex";
}
