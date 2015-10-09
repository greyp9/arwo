package io.github.greyp9.arwo.core.xsd.source;

import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.factory.XsdAtomFactory;
import io.github.greyp9.arwo.core.xslt.XsltX;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

public class SchemaCollectionFactory {
    private final URL urlCatalog;
    private final URL urlTransform;
    private final Map<String, SchemaAtom> schemaAtoms;

    public SchemaCollectionFactory(final URL urlCatalog) {
        this(urlCatalog, null);
    }

    public SchemaCollectionFactory(final URL urlCatalog, final URL urlTransform) {
        this.urlCatalog = urlCatalog;
        this.urlTransform = urlTransform;
        this.schemaAtoms = new TreeMap<String, SchemaAtom>();
    }

    public final SchemaCollection create(final URL urlInitial) throws IOException {
        final XsltX xsltX = createTransform();
        final String targetNamespace = add(urlInitial, xsltX);
        return new SchemaCollection(targetNamespace, schemaAtoms);
    }

    private String add(final URL url, final XsltX xsltX) throws IOException {
        String targetNamespace = null;
        final String catalogURL = URLCodec.toExternalForm(urlCatalog);
        final String initialURL = URLCodec.toExternalForm(url);
        final String uri = initialURL.replace(catalogURL, "");
        if (!schemaAtoms.containsKey(uri)) {
            // get schema content
            byte[] bytesXsd = StreamU.read(url);
            // optional transform
            if (xsltX != null) {
                bytesXsd = xsltX.transform(bytesXsd);
            }
            // default XSLT for XSD
            final URL urlAugmentXSLT = new URL(initialURL.replace(".xsd", ".xslt"));
            final byte[] xslt = StreamU.readSafe(urlAugmentXSLT);
            if (xslt != null) {
                final XsltX xsltAugment = new XsltX(xslt);
                bytesXsd = xsltAugment.transform(bytesXsd);
            }
            // load into DOM
            final Document document = DocumentU.toDocument(bytesXsd);
            final Element element = document.getDocumentElement();
            // register content
            final XsdAtom xsdAtom = XsdAtomFactory.create(element, null);
            targetNamespace = ElementU.getAttribute(xsdAtom.getElement(), XsdU.TARGET_NAMESPACE);
            final QName name = QNameU.getQName(targetNamespace, element.getLocalName(), null);
            final SchemaAtom schemaAtom = new SchemaAtom(url, name, xsdAtom);
            schemaAtoms.put(uri, schemaAtom);
            // import dependencies
            doImport(url, xsdAtom, xsltX);
            doInclude(url, xsdAtom, xsltX);
        }
        return targetNamespace;
    }

    private void doImport(final URL url, final XsdAtom xsdAtom, final XsltX xsltX) throws IOException {
        final Collection<Element> children = ElementU.getChildren(xsdAtom.getElement(), XsdU.IMPORT);
        for (final Element child : children) {
            final String schemaLocation = ElementU.getAttribute(child, XsdU.SCHEMA_LOCATION);
            final URL urlImport = URLCodec.resolve(url, schemaLocation);
            add(urlImport, xsltX);
        }
    }

    private void doInclude(final URL url, final XsdAtom xsdAtom, final XsltX xsltX) throws IOException {
        final Collection<Element> children = ElementU.getChildren(xsdAtom.getElement(), XsdU.INCLUDE);
        for (final Element child : children) {
            final String schemaLocation = ElementU.getAttribute(child, XsdU.SCHEMA_LOCATION);
            final URL urlInclude = URLCodec.resolve(url, schemaLocation);
            add(urlInclude, xsltX);
        }
    }

    private XsltX createTransform() throws IOException {
        return (urlTransform == null) ? null : new XsltX(StreamU.read(urlTransform));
    }
}
