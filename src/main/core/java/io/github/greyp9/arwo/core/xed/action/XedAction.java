package io.github.greyp9.arwo.core.xed.action;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.Locale;

public class XedAction {
    private final Xed xed;

    public final Xed getXed() {
        return xed;
    }

    public XedAction(final QName qname, final Locale locale) throws IOException {
        final URL url = ResourceU.resolve(App.Actions.XSD);
        final XsdTypes xsdTypes = new XsdTypes(url);
        final DocumentFactory documentFactory = new DocumentFactory(xsdTypes.getTypeDefinitions());
        final Document document = documentFactory.generateEmpty(qname);
        this.xed = new Xed(document, xsdTypes, locale);
    }

    public final XedCursor getCursor() {
        return new XedNav(xed).getRoot();
    }

    public final Xed update(final NameTypeValues nameTypeValues) throws IOException {
        final XedCursor cursor = getCursor();
        final ValueInstance valueInstance = ValueInstance.create(cursor.getTypeInstance(), nameTypeValues);
        new OpUpdate(null, xed.getXsdTypes()).apply(cursor.getElement(), valueInstance);
        return xed;
    }
}
