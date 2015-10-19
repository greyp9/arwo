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

    public Locale getLocale() {
        return locale;
    }

    public ResourceBundle getBundle(String uri) {
        return bundles.get(uri);
    }

    public XsdBundles(XsdTypes xsdTypes, Locale locale) {
        this.locale = locale;
        this.bundles = new TreeMap<String, ResourceBundle>();
        if (locale != null) {
            update(xsdTypes, locale);
        }
    }

    public void update(XsdTypes xsdTypes, Locale locale) {
        SchemaCollection schemaCollection = xsdTypes.getTypeDefinitions().getTypeComponents().getSchemaCollection();
        Collection<SchemaAtom> schemaAtoms = schemaCollection.getSchemas().values();
        for (SchemaAtom schemaAtom : schemaAtoms) {
            update(schemaAtom, locale);
        }
    }

    private void update(SchemaAtom schemaAtom, Locale locale) {
        String namespaceURI = schemaAtom.getQName().getNamespaceURI();
        URL url = schemaAtom.getUrl();
        String urlExternalForm = url.toExternalForm();
        FileX fileX = new FileX(urlExternalForm);
        String urlExternalFormParent = fileX.getFolderSlash();
        String resourceName = fileX.getFilename();
        String resourceNameBare = resourceName.replace(fileX.getExtensionDot(), "");
        try {
            URL[] urls = new URL[] { new URL(urlExternalFormParent) };
            ClassLoader classLoader = new URLClassLoader(urls);
            ResourceBundle rb = ResourceBundle.getBundle(resourceNameBare, locale, classLoader);
            bundles.put(namespaceURI, rb);
        } catch (IOException e) {
            getClass();  // ignore, type labels won't be localized
        } catch (MissingResourceException e) {
            getClass();  // ignore, type labels won't be localized
        }

    }
}
