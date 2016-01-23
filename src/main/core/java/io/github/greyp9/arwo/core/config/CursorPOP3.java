package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import org.w3c.dom.Element;

import java.io.IOException;

// i18nf
public class CursorPOP3 {
    private final XedCursor cursor;

    public final XedCursor getCursor() {
        return cursor;
    }

    public CursorPOP3(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public CursorPOP3(final Xed xed, final String name) throws IOException {
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

    public final String getProtocol() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PROTOCOL));
    }

    public final String getHost() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.HOST));
    }

    public final Integer getPort() {
        return TypeU.toInteger(cursor.getValue(cursor.getChildInstance(App.Settings.PORT)));
    }

    public final String getUser() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.USER));
    }

    public final String getPassword() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PASSWORD));
    }

    public final String getCertificate() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.CERTIFICATE));
    }

    private static class Const {
        private static final String SELECT_BY_NAME = "/app:app/app:pop3Servers/app:server[app:name/text()='%s']";
    }
}
