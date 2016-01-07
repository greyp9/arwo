package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;

import java.io.IOException;
import java.util.Locale;

public class XedActionFolderNew extends XedAction {

    public XedActionFolderNew(final Locale locale) throws IOException {
        super(App.Actions.QNAME_FOLDER_NEW, locale);
    }
}
