package io.github.greyp9.arwo.core.xed.model;

import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xed.op.OpClone;
import io.github.greyp9.arwo.core.xed.op.OpCreate;
import io.github.greyp9.arwo.core.xed.op.OpDelete;
import io.github.greyp9.arwo.core.xed.op.OpMove;
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
import java.util.Locale;
import java.util.ResourceBundle;

public class Xed {
    private final Document document;
    private final XsdTypes xsdTypes;
    private final XsdBundles xsdBundles;

    public final Document getDocument() {
        return document;
    }

    public final XsdTypes getXsdTypes() {
        return xsdTypes;
    }

/*
    public final XsdBundles getXsdBundles() {
        return xsdBundles;
    }
*/

    public Xed(final Document document, final XsdTypes xsdTypes) {
        this(document, xsdTypes, Locale.getDefault());
    }

    public Xed(final Document document, final XsdTypes xsdTypes, final Locale locale) {
        this.document = document;
        this.xsdTypes = xsdTypes;
        this.xsdBundles = new XsdBundles(xsdTypes, locale);
    }

    public final Bundle getBundle() {
        final ResourceBundle bundleCore = new AppText(xsdBundles.getLocale()).getBundleCore();
        final ResourceBundle bundleRoot = getRootBundle();
        return new Bundle(bundleRoot, bundleCore);
    }

    public final Locale getLocale() {
        return xsdBundles.getLocale();
    }

    public final ResourceBundle getRootBundle() {
        return getBundle(document.getDocumentElement().getNamespaceURI());
    }

    public final ResourceBundle getBundle(final String uri) {
        return xsdBundles.getBundle(uri);
    }

    public final XPather getXPather() {
        return new XPather(document, xsdTypes.getContext());
    }

    public final Collection<String> validate() throws IOException {
        final Validator validator = new Validator(xsdTypes.getUrlInitial());
        return validator.validate(DocumentU.toXml(document));
    }

    public final Element create(final Element parent, final ValueInstance valueInstance) {
        return new OpCreate(xsdTypes).apply(parent, valueInstance);
    }

    public final Element update(final Element element, final ValueInstance valueInstance) {
        return new OpUpdate(xsdTypes).apply(element, valueInstance);
    }

    public final Element delete(final Element element) {
        return new OpDelete().apply(element);
    }

    public final Element clone(final Element element) throws IOException {
        return new OpClone().apply(element);
    }

    public final Element moveUp(final Element element) {
        return new OpMove(xsdTypes).moveUp(element);
    }

    public final Element moveDown(final Element element) {
        return new OpMove(xsdTypes).moveDown(element);
    }
}
