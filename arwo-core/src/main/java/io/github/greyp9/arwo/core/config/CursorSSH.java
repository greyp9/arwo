package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.app.App;
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
public final class CursorSSH {
    private final XedCursor cursor;

    public XedCursor getCursor() {
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

    public String getName() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.NAME));
    }

    public boolean isEnabled() {
        return TypeU.toBooleanP(cursor.getValue(cursor.getChildInstance(App.Settings.ENABLED)));
    }

    public String getComment() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.COMMENT));
    }

    public String getHost() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.HOST));
    }

    public String getPort() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PORT));
    }

    public String getProxy() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PROXY));
    }

    public String getTerm() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.TERM));
    }

    public String getUser() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.USER));
    }

    public String getAuthentication() {
        String authentication = null;
        final XsdBundle bundle = cursor.getXed().getXsdBundle();
        final TypeInstance parentInstance = cursor.getTypeInstance();
        if (Value.isData(getPassword())) {
            authentication = bundle.getLabel(parentInstance, parentInstance.getInstance(App.Settings.AUTH_PASSWORD));
        } else if (Value.isData(getPrivateKey())) {
            authentication = bundle.getLabel(parentInstance, parentInstance.getInstance(App.Settings.AUTH_PUBLIC_KEY));
        }
        return authentication;
    }

    public String getPassword() {
        return cursor.getValue(cursor.getChildInstance(
                App.Settings.AUTH_PASSWORD).getInstance(App.Settings.PASSWORD));
    }

    public String getPrivateKey() {
        return cursor.getValue(cursor.getChildInstance(
                App.Settings.AUTH_PUBLIC_KEY).getInstance(App.Settings.PRIVATE_KEY));
    }

    public String getAlgorithm() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.ALGORITHM));
    }

    public String getPublicKey() {
        return cursor.getValue(cursor.getChildInstance(App.Settings.PUBLIC_KEY));
    }

    private static class Const {
        private static final String SELECT_BY_NAME = "/app:app/app:sshServers/app:server[app:name/text()='%s']";
    }
}
