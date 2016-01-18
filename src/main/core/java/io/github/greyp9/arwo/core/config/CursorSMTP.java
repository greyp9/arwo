package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import org.w3c.dom.Element;

import java.io.IOException;

// i18nf
public class CursorSMTP {
    private final XedCursor cursor;

    public final XedCursor getCursor() {
        return cursor;
    }

    public CursorSMTP(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public CursorSMTP(final Xed xed, final String name) throws IOException {
        final String xpath = String.format(Const.SELECT_BY_NAME, name);
        final Element element = xed.getXPather().getElement(xpath);
        this.cursor = (element == null) ? null : new XedNav(xed).find(element);
    }

    public final String getName() {
        return cursor.getValue(cursor.getChildInstance("name"));
    }

    public final boolean isEnabled() {
        return TypeU.toBooleanP(cursor.getValue(cursor.getChildInstance("enabled")));
    }

    public final String getComment() {
        return cursor.getValue(cursor.getChildInstance("comment"));
    }

    public final String getProtocol() {
        return cursor.getValue(cursor.getChildInstance("protocol"));
    }

    public final String getHost() {
        return cursor.getValue(cursor.getChildInstance("host"));
    }

    public final Integer getPort() {
        return TypeU.toInteger(cursor.getValue(cursor.getChildInstance("port")));
    }

    public final String getUser() {
        return cursor.getValue(cursor.getChildInstance("user"));
    }

    public final String getPassword() {
        return cursor.getValue(cursor.getChildInstance("password"));
    }

    public final String getCertificate() {
        return cursor.getValue(cursor.getChildInstance("certificate"));
    }

    private static class Const {
        private static final String SELECT_BY_NAME = "/app:app/app:smtpServers/app:server[app:name/text()='%s']";
    }
}
