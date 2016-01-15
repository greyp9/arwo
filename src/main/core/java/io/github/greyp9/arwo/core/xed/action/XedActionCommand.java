package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.model.Xed;

import java.io.IOException;
import java.util.Locale;

public class XedActionCommand extends XedAction {

    public XedActionCommand(final Locale locale) throws IOException {
        super(App.Actions.QNAME_COMMAND, locale);
    }

    public final String getCommand(final NameTypeValues httpArguments) throws IOException {
        final Xed xed = super.update(httpArguments);
        return xed.getXPather().getText("/action:command/action:command");  // i18n
    }
}
