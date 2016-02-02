package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.xed.model.XedFactory;

import java.io.IOException;

public class XedActionFolderNew extends XedAction {

    public XedActionFolderNew(final XedFactory factory) throws IOException {
        super(App.Actions.QNAME_FOLDER_NEW, factory, null);
    }
}
