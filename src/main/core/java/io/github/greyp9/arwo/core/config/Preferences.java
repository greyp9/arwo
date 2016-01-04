package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import org.w3c.dom.Element;

import java.io.IOException;

public class Preferences {
    private final XedCursor cursor;

    public final XedCursor getCursor() {
        return cursor;
    }

    public Preferences(final Xed xed) throws IOException {
        final Element element = xed.getXPather().getElement(Const.PREFERENCES);
        this.cursor = new XedNav(xed).find(element);
    }

    public final String getMIMEType(final String path) throws IOException {
        final String extensionFile = Value.defaultOnNull(new FileX(path).getExtension(), "");
        final String xpath = String.format(Const.MIME_TYPE_FOR_EXT, extensionFile);
        final String mimeType = cursor.getXed().getXPather().getText(xpath);
        return Value.defaultOnEmpty(mimeType, getDefaultMIMEType());
    }

    public final String getDefaultMIMEType() {
        final XedCursor cursorMIMETypes = new XedNav(cursor.getXed()).findChild("mimeTypes", cursor);
        return cursorMIMETypes.getValue(cursorMIMETypes.getChildInstance("mimeDefaultType"));
    }

    private static class Const {
        private static final String PREFERENCES = "/app:app/app:preferences";
        private static final String MIME_TYPES = PREFERENCES + "/app:mimeTypes";
        private static final String MIME_TYPE_FOR_EXT = MIME_TYPES + "/app:mimeType[app:extension='%s']/app:type";
    }
}
