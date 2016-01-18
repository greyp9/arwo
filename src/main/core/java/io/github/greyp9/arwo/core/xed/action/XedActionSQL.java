package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.model.Xed;

import java.io.IOException;
import java.util.Locale;

public class XedActionSQL extends XedAction {

    public XedActionSQL(final Locale locale) throws IOException {
        super(App.Actions.QNAME_SQL, locale);
    }

    public final String getSQL(final NameTypeValues httpArguments) throws IOException {
        final Xed xed = super.update(httpArguments);
        return xed.getXPather().getText("/action:sql/action:sql");  // i18n
    }
}
