package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.Locale;

public class XedAction {
    private final Xed xed;
    private final XedFactory factory;

    public final XedFactory getFactory() {
        return factory;
    }

    public final Xed getXed() {
        return xed;
    }

    public XedAction(final QName qname, final XedFactory factory, final Locale locale) throws IOException {
        final URL url = ResourceU.resolve(App.Actions.XSD);
        this.xed = factory.generateEmpty(url, qname, locale);
        this.factory = factory;
    }

    public final Xed getXedUI(final Locale locale) throws IOException {
        return new Xed(xed.getDocument(), xed.getXsdTypes(), factory.getXsdBundle(xed.getXsdTypes(), locale));
    }

    public final Xed update(final NameTypeValues nameTypeValues) throws IOException {
        final XedCursor cursor = new XedNav(xed).getRoot();
        final ValueInstance valueInstance = ValueInstance.create(cursor.getTypeInstance(), nameTypeValues);
        new OpUpdate(null, xed).apply(cursor.getElement(), valueInstance);
        return xed;
    }
}
