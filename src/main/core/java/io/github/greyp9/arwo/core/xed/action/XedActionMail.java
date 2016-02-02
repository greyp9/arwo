package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.xed.model.XedFactory;

import java.io.IOException;

public class XedActionMail extends XedAction {

    public XedActionMail(final XedFactory factory) throws IOException {
        super(App.Actions.QNAME_MAIL, factory, null);
    }
}
