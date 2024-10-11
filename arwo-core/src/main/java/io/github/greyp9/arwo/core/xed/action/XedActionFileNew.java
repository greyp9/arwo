package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.xed.model.XedFactory;

import java.io.IOException;

public class XedActionFileNew extends XedAction {

    public XedActionFileNew(final XedFactory factory) throws IOException {
        super(App.Actions.QNAME_FILE_NEW, factory, null);
    }
}
