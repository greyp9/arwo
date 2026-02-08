package io.github.greyp9.arwo.core.xed.bundle;

import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.TreeMap;

public class XsdBundles {
    private final Locale locale;
    private final Map<String, ResourceBundle> bundles;

    public final Locale getLocale() {
        return locale;
    }

    public final ResourceBundle getBundle(final String uri) {
        return ((uri == null) ? null : bundles.get(uri));
    }

    public XsdBundles(final XsdTypes xsdTypes, final Locale locale) {
        this.locale = locale;
        this.bundles = new TreeMap<String, ResourceBundle>();
        if (locale != null) {
            update(xsdTypes, locale);
        }
    }

    public final void update(final XsdTypes xsdTypes, final Locale localeUpdate) {
        final SchemaCollection schemaCollection =
                xsdTypes.getTypeDefinitions().getTypeComponents().getSchemaCollection();
        final Collection<SchemaAtom> schemaAtoms = schemaCollection.getSchemas().values();
        for (final SchemaAtom schemaAtom : schemaAtoms) {
            update(schemaAtom, localeUpdate);
        }
    }

    private void update(final SchemaAtom schemaAtom, final Locale localeUpdate) {
        final String namespaceURI = schemaAtom.getQName().getNamespaceURI();
        final URL url = schemaAtom.getURL();
        final String urlExternalForm = url.toExternalForm();
        final FileX fileX = new FileX(urlExternalForm);
        final String urlExternalFormP = fileX.getFolderSlash();
        final String resourceName = fileX.getFilename();
        final String resourceNameBare = resourceName.replace(fileX.getExtensionDot(), "");
        try {
            final URL[] urls = new URL[] { new URL(urlExternalFormP) };
            final ClassLoader classLoader = new URLClassLoader(urls);
            final ResourceBundle rb = ResourceBundle.getBundle(resourceNameBare, localeUpdate, classLoader);
            bundles.put(namespaceURI, rb);
        } catch (IOException | MissingResourceException ignored) {
            // ignore, type labels won't be localized
        }
    }
}
