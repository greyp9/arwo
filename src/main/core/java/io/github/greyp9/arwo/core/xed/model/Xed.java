package io.github.greyp9.arwo.core.xed.model;

import io.github.greyp9.arwo.core.xed.op.OpCreate;
import io.github.greyp9.arwo.core.xed.op.OpDelete;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.validate.Validator;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;

public class Xed {
    private final Document document;
    private final XsdTypes xsdTypes;

    public final Document getDocument() {
        return document;
    }

    public final XsdTypes getXsdTypes() {
        return xsdTypes;
    }

    public Xed(final Document document, final XsdTypes xsdTypes) {
        this.document = document;
        this.xsdTypes = xsdTypes;
    }

    public final XPather getXPather() {
        return new XPather(document, xsdTypes.getContext());
    }

    public final Collection<String> validate() throws IOException {
        final Validator validator = new Validator(xsdTypes.getUrlInitial());
        return validator.validate(DocumentU.toXml(document));
    }

    public final Element create(final Element parent, final ValueInstance valueInstance) {
        return new OpCreate(parent).apply(valueInstance);
    }

    public final Element update(final Element element, final ValueInstance valueInstance) {
        return new OpUpdate(element).apply(valueInstance);
    }

    public final Element delete(final Element element) {
        return new OpDelete(element).apply();
    }
}
