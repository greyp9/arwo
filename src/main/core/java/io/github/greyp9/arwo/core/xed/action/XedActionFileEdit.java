package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;

import java.io.IOException;
import java.util.Locale;

public class XedActionFileEdit extends XedAction {

    public XedActionFileEdit(final Locale locale) throws IOException {
        super(App.Actions.QNAME_FILE_EDIT, locale);
    }
}
