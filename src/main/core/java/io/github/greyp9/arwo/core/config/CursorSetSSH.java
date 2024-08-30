package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

// i18nf
public final class CursorSetSSH {
    private final Collection<XedCursor> cursors;

    public Collection<XedCursor> getCursors() {
        return cursors;
    }

    public CursorSetSSH(final Xed xed) throws IOException {
        this.cursors = new ArrayList<XedCursor>();
        final XedNav nav = new XedNav(xed);
        final List<Element> elements = xed.getXPather().getElements(Const.SELECT_ALL);
        for (final Element element : elements) {
            cursors.add(nav.find(element));
        }
    }

    private static class Const {
        private static final String SELECT_ALL = "/app:app/app:sshServers/app:server";
    }
}
