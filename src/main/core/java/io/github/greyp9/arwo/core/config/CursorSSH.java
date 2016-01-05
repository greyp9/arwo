package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.lang.TypeU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.io.IOException;

// i18nf
public class CursorSSH {
    private final XedCursor cursor;

    public final XedCursor getCursor() {
        return cursor;
    }

    public CursorSSH(final XedCursor cursor) {
        this.cursor = cursor;
    }

    public CursorSSH(final Xed xed, final String name) throws IOException {
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

    public final String getHost() {
        return cursor.getValue(cursor.getChildInstance("host"));
    }

    public final String getPort() {
        return cursor.getValue(cursor.getChildInstance("port"));
    }

    public final String getUser() {
        return cursor.getValue(cursor.getChildInstance("user"));
    }

    public final String getAuthentication() {
        String authentication = null;
        final XsdBundle bundle = cursor.getXed().getXsdBundle();
        final TypeInstance parentInstance = cursor.getTypeInstance();
        if (Value.isData(getPassword())) {
            authentication = bundle.getLabel(parentInstance, parentInstance.getInstance("authPassword"));
        } else if (Value.isData(getPrivateKey())) {
            authentication = bundle.getLabel(parentInstance, parentInstance.getInstance("authPublicKey"));
        }
        return authentication;
    }

    public final String getPassword() {
        return cursor.getValue(cursor.getChildInstance("authPassword").getInstance("password"));
    }

    public final String getPrivateKey() {
        return cursor.getValue(cursor.getChildInstance("authPublicKey").getInstance("privateKey"));
    }

    public final String getAlgorithm() {
        return cursor.getValue(cursor.getChildInstance("algorithm"));
    }

    public final String getPublicKey() {
        return cursor.getValue(cursor.getChildInstance("publicKey"));
    }

    private static class Const {
        private static final String SELECT_BY_NAME = "/app:app/app:sshServers/app:server[app:name/text()='%s']";
    }
}
