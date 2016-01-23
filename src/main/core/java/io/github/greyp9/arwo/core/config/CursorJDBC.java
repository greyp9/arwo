package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import org.w3c.dom.Element;

import java.io.IOException;

// i18nf
public class CursorJDBC {
    private final XedCursor cursor;

    public final XedCursor getCursor() {
        return cursor;
    }

    public CursorJDBC(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public CursorJDBC(final Xed xed, final String name) throws IOException {
        final String xpath = String.format(Const.SELECT_BY_NAME, name);
        final Element element = xed.getXPather().getElement(xpath);
        this.cursor = (element == null) ? null : new XedNav(xed).find(element);
    }

    public final String getName() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.NAME));
    }

    public final boolean isEnabled() {
        return TypeU.toBooleanP(cursor.getValue(cursor.getChildInstance(App.Settings.ENABLED)));
    }

    public final String getComment() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.COMMENT));
    }

    public final String getDriverClass() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.DRIVER_CLASS));
    }

    public final String getURL() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.JDBC_URL));
    }

    public final String getUser() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.USER));
    }

    public final String getPassword() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PASSWORD));
    }

    private static class Const {
        private static final String SELECT_BY_NAME = "/app:app/app:jdbcServers/app:server[app:name/text()='%s']";
    }
}
