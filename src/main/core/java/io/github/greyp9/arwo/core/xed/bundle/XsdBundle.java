package io.github.greyp9.arwo.core.xed.bundle;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Logic to provide a UI labels for XSD TypeInstance and DataType atoms.
 */
public class XsdBundle {
    private final XsdBundles xsdBundles;

    public final XsdBundles getXsdBundles() {
        return xsdBundles;
    }

    public XsdBundle(final XsdBundles xsdBundles) {
        this.xsdBundles = xsdBundles;
    }

    public final Locale getLocale() {
        return xsdBundles.getLocale();
    }

    public final String getLabel(final TypeInstance typeInstance) {
        final String uri = typeInstance.getURI();
        final String id = typeInstance.getID();
        final ResourceBundle bundle = xsdBundles.getBundle(uri);
        return new Bundle(bundle).getString(id);
    }

    public final String getLabelEnum(
            final TypeInstance typeInstance, final TypeInstance typeInstanceChild, final String enumValue) {
        final String uri = typeInstance.getURI();
        final String id = Value.join(Http.Token.DOT,
                typeInstanceChild.getID(typeInstance), "enum", enumValue); // i18n internal
        final ResourceBundle bundle = xsdBundles.getBundle(uri);
        return new Bundle(bundle).getString(id);
    }

    public final String getLabelEnumCompact(
            final TypeInstance typeInstance, final TypeInstance typeInstanceChild, final String enumValue) {
        final String uri = typeInstance.getURI();
        final String id = Value.join(Http.Token.DOT,
                typeInstanceChild.getID(typeInstance), "enum", "c", enumValue);  // i18n internal
        final ResourceBundle bundle = xsdBundles.getBundle(uri);
        return new Bundle(bundle).getString(id);
    }

    public final String getLabel(final TypeInstance typeInstance, final TypeInstance typeInstanceChild) {
        return ((typeInstance == null) ? getLabel(typeInstanceChild) : getLabelSafe(typeInstance, typeInstanceChild));
    }

    public final String getDetail(final TypeInstance typeInstance) {
        final String uri = typeInstance.getURI();
        final String id = typeInstance.getID() + ".DETAIL";
        final ResourceBundle bundle = xsdBundles.getBundle(uri);
        return new Bundle(bundle).getString(id);
    }

    private String getDetailSafe(final TypeInstance typeInstance, final TypeInstance typeInstanceChild) {
        final String uri = typeInstance.getURI();
        final String id = typeInstanceChild.getID(typeInstance) + ".DETAIL";
        final ResourceBundle bundle = xsdBundles.getBundle(uri);
        return new Bundle(bundle).getString(id);
    }

    public final String getDetail(final TypeInstance typeInstance, final TypeInstance typeInstanceChild) {
        return ((typeInstance == null) ? getDetail(typeInstanceChild) : getDetailSafe(typeInstance, typeInstanceChild));
    }

    private String getLabelSafe(final TypeInstance typeInstance, final TypeInstance typeInstanceChild) {
        final String uri = typeInstance.getURI();
        final String id = typeInstanceChild.getID(typeInstance);
        final ResourceBundle bundle = xsdBundles.getBundle(uri);
        return new Bundle(bundle).getString(id);
    }
}
