package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;

import java.io.IOException;

public class XedActionCommand extends XedAction {

    public XedActionCommand(final XedFactory xedFactory) throws IOException {
        super(App.Actions.QNAME_COMMAND, xedFactory, null);
    }

    public final String getCommand(final NameTypeValues httpArguments) throws IOException {
        final Xed xed = super.update(httpArguments);
        return xed.getXPather().getText("/action:command/action:command");  // i18n xpath
    }
}
