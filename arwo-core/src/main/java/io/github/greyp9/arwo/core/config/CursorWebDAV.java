package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import org.w3c.dom.Element;

import java.io.IOException;

// i18nf
public final class CursorWebDAV {
    private final XedCursor cursor;

    public XedCursor getCursor() {
        return cursor;
    }

    public CursorWebDAV(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public CursorWebDAV(final Xed xed, final String name) throws IOException {
        final String xpath = String.format(Const.SELECT_BY_NAME, name);
        final Element element = xed.getXPather().getElement(xpath);
        this.cursor = (element == null) ? null : new XedNav(xed).find(element);
    }

    public String getName() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.NAME));
    }

    public boolean isEnabled() {
        return TypeU.toBooleanP(cursor.getValue(cursor.getChildInstance(App.Settings.ENABLED)));
    }

    public String getComment() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.COMMENT));
    }

    public String getProtocol() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PROTOCOL));
    }

    public String getHost() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.HOST));
    }

    public String getPort() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PORT));
    }

    public String getUser() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.USER));
    }

    public String getPassword() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PASSWORD));
    }

    public String getCertificate() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.CERTIFICATE));
    }

    private static class Const {
        private static final String SELECT_BY_NAME = "/app:app/app:webdavServers/app:server[app:name/text()='%s']";
    }
}