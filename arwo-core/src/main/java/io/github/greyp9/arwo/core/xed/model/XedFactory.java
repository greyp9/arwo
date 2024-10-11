package io.github.greyp9.arwo.core.xed.model;

import io.github.greyp9.arwo.core.time.Stopwatch;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;

public class XedFactory {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final Map<String, XsdTypes> xsdTypesMap;
    private final Map<String, XsdBundle> xsdBundlesMap;

    public XedFactory() {
        this.xsdTypesMap = new TreeMap<String, XsdTypes>();
        this.xsdBundlesMap = new TreeMap<String, XsdBundle>();
    }

    public XedFactory(final XedFactory factory) {
        this();
        for (final Map.Entry<String, XsdTypes> entryT : factory.xsdTypesMap.entrySet()) {
            this.xsdTypesMap.put(entryT.getKey(), entryT.getValue());
        }
        for (final Map.Entry<String, XsdBundle> entryB : factory.xsdBundlesMap.entrySet()) {
            this.xsdBundlesMap.put(entryB.getKey(), entryB.getValue());
        }
    }

    public final Xed generateEmpty(final URL urlInitial, final QName qname, final Locale locale) throws IOException {
        final XsdTypes xsdTypes = getXsdTypes(urlInitial, null, null);
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final XsdBundle xsdBundle = (locale == null) ? null : getXsdBundle(xsdTypes, locale);
        return new Xed(document, xsdTypes, xsdBundle);
    }

    public final Xed getXedUI(final Xed xed, final Locale locale) throws IOException {
        return new Xed(xed.getDocument(), xed.getXsdTypes(), getXsdBundle(xed.getXsdTypes(), locale));
    }

    public final XsdTypes getXsdTypes(
            final URL urlInitial, final URL urlCatalog, final URL urlTransform) throws IOException {
        synchronized (xsdTypesMap) {
            final String key = getTypesKey(urlInitial, urlCatalog, urlTransform);
            XsdTypes xsdTypes = xsdTypesMap.get(key);
            if (xsdTypes == null) {
                final Stopwatch stopwatch = new Stopwatch(key);
                xsdTypes = new XsdTypes(urlInitial, urlCatalog, urlTransform);
                xsdTypesMap.put(key, xsdTypes);
                stopwatch.lap();
                logger.finest(stopwatch.toString());
            }
            return xsdTypes;
        }
    }

    public final XsdBundle getXsdBundle(final XsdTypes xsdTypes, final Locale locale) throws IOException {
        synchronized (xsdBundlesMap) {
            final String key = getBundleKey(xsdTypes, locale);
            XsdBundle xsdBundle = xsdBundlesMap.get(key);
            if (xsdBundle == null) {
                final Stopwatch stopwatch = new Stopwatch(key);
                xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, locale));
                xsdBundlesMap.put(key, xsdBundle);
                stopwatch.lap();
                logger.finest(stopwatch.toString());
            }
            return xsdBundle;
        }
    }

    private static String getTypesKey(final URL urlInitial, final URL urlCatalog, final URL urlTransform) {
        return String.format("[%s][%s][%s]", URLCodec.toExternalForm(urlInitial),
                URLCodec.toExternalForm(urlCatalog), URLCodec.toExternalForm(urlTransform));
    }

    private static String getBundleKey(final XsdTypes xsdTypes, final Locale locale) {
        final String key = getTypesKey(xsdTypes.getUrlInitial(), xsdTypes.getUrlCatalog(), xsdTypes.getUrlTransform());
        return String.format("[%s][%s]", key, ((locale == null) ? null : locale.toString()));
    }
}
