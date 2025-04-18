package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;

import java.io.IOException;

public class XedActionStdin extends XedAction {

    public XedActionStdin(final XedFactory xedFactory) throws IOException {
        super(App.Actions.QNAME_STDIN, xedFactory, null);
    }

    public final String getStdin(final NameTypeValues httpArguments) throws IOException {
        final Xed xed = super.update(httpArguments);
        return xed.getXPather().getText("/action:stdin/action:stdin");  // i18n xpath
    }
}
