package io.github.greyp9.arwo.core.config;

import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Locale;

// i18nf
public final class Preferences {
    private final XedCursor cursor;

    public XedCursor getCursor() {
        return cursor;
    }

    public Preferences(final Xed xed) throws IOException {
        final Element element = xed.getXPather().getElement(Const.PREFERENCES);
        this.cursor = new XedNav(xed).find(element);
    }

    public String getSetting(final String xpath) throws IOException {
        return cursor.getXed().getXPather().getText(Const.PREFERENCES + xpath);
    }

    public String getMIMEType(final String path) throws IOException {
        final String extensionFile = Value.defaultOnNull(new FileX(path).getExtension(), "");
        final String extensionToLower = extensionFile.toLowerCase(Locale.getDefault());
        final String xpath = String.format(Const.MIME_TYPE_FOR_EXT, extensionToLower);
        final String mimeType = cursor.getXed().getXPather().getText(xpath);
        return Value.defaultOnEmpty(mimeType, getDefaultMIMEType());
    }

    public String getDefaultMIMEType() {
        final XedCursor cursorMIMETypes = new XedNav(cursor.getXed()).findChild("mimeTypes", cursor);
        return cursorMIMETypes.getValue(cursorMIMETypes.getChildInstance("mimeDefaultType"));
    }

    public String getTZ() {
        final XedCursor cursorIt = new XedNav(cursor.getXed()).findChild("localization", cursor);
        return cursorIt.getValue(cursorIt.getChildInstance("tz"));
    }

    public String getDateFormat() {
        final XedCursor cursorIt = new XedNav(cursor.getXed()).findChild("localization", cursor);
        return cursorIt.getValue(cursorIt.getChildInstance("dateFormat"));
    }

    public String getLanguage() {
        final XedCursor cursorLocal = new XedNav(cursor.getXed()).findChild("localization", cursor);
        return cursorLocal.getValue(cursorLocal.getChildInstance("language"));
    }

    public int getTablePageSize() {
        final XedCursor cursorTable = new XedNav(cursor.getXed()).findChild("table", cursor);
        return NumberU.toInt(cursorTable.getValue(cursorTable.getChildInstance("pageSize")), Const.PAGE_SIZE_TABLE);
    }

    public String getIconColor() {
        final XedCursor cursorLocal = new XedNav(cursor.getXed()).findChild("ui", cursor);
        return cursorLocal.getValue(cursorLocal.getChildInstance("shortcut"));
    }

    public String getTheme() {
        final XedCursor cursorLocal = new XedNav(cursor.getXed()).findChild("ui", cursor);
        return cursorLocal.getValue(cursorLocal.getChildInstance("theme"));
    }

    private static class Const {
        private static final String PREFERENCES = "/app:app/app:preferences";
        private static final String MIME_TYPES = PREFERENCES + "/app:mimeTypes";
        private static final String MIME_TYPE_FOR_EXT = MIME_TYPES + "/app:mimeType[app:extension='%s']/app:type";
        //private static final String LOCALIZATION = PREFERENCES + "/app:localization";
        //private static final String TZ = LOCALIZATION + "/app:tz";
        //private static final String DATE_FORMAT = LOCALIZATION + "/app:dateFormat";
        //private static final String LANGUAGE = LOCALIZATION + "/app:language";
        private static final int PAGE_SIZE_TABLE = 30;
    }
}
