package io.github.greyp9.arwo.core.xsd.source;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtomFactory;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xslt.XsltX;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;

public class SchemaCollectionFactory {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final URL urlCatalog;
    private final URL urlTransform;
    private final XsdAtomFactory atomFactory;
    private final Map<String, SchemaAtom> schemaAtoms;

    public SchemaCollectionFactory(final URL urlCatalog) {
        this(urlCatalog, null);
    }

    public SchemaCollectionFactory(final URL urlCatalog, final URL urlTransform) {
        this.urlCatalog = urlCatalog;
        this.urlTransform = urlTransform;
        this.atomFactory = new XsdAtomFactory();
        this.schemaAtoms = new TreeMap<String, SchemaAtom>();
    }

    public final SchemaCollection create(final URL urlInitial) throws IOException {
        final XsltX xsltX = createTransform();
        final SchemaAtom schemaAtom = add(null, urlInitial, xsltX);
        return new SchemaCollection(schemaAtom, schemaAtoms);
    }

    private SchemaAtom add(final String targetNamespaceIn, final URL url, final XsltX xsltX) throws IOException {
        SchemaAtom schemaAtom = null;
        logger.finest(url.toExternalForm());
        final String catalogURL = URLCodec.toExternalForm(urlCatalog);
        final String initialURL = URLCodec.toExternalForm(url);
        final String uri = ((catalogURL == null) ? initialURL : initialURL.replace(catalogURL, ""));
        final String protocol = url.getProtocol();
        final boolean isLocal = (("file".equals(protocol)) || ("jar".equals(protocol)));  // i18n internal
        final boolean isLoaded = schemaAtoms.containsKey(uri);
        if (isLocal && (!isLoaded)) {
            // get schema content
            byte[] bytesXsd = StreamU.read(url);
            // optional transform
            if (xsltX != null) {
                bytesXsd = xsltX.transform(bytesXsd);
            }
            logger.finest(UTF8Codec.toString(bytesXsd));
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
            final String targetNamespace = ElementU.getAttribute(
                    element, XsdU.TARGET_NAMESPACE, XsdU.NS_URI_NULL);
            if ((targetNamespaceIn != null) && (!targetNamespaceIn.equals(targetNamespace))) {
                logger.warning(String.format("%s != %s", targetNamespaceIn, targetNamespace));
            }
            // register content
            final XsdAtom xsdAtom = atomFactory.create(element, null);
            final QName name = QNameU.getQName(targetNamespace, element.getLocalName(), null);
            schemaAtom = new SchemaAtom(url, name, xsdAtom);
            schemaAtoms.put(uri, schemaAtom);
            //schemaAtoms.put(targetNamespace, schemaAtom);  // why this? (duplicate)
            // import dependencies
            doImport(url, xsdAtom, xsltX);
            doInclude(url, xsdAtom, xsltX);
        }
        return schemaAtom;
    }

    private void doImport(final URL url, final XsdAtom xsdAtom, final XsltX xsltX) throws IOException {
        final Collection<Element> children = ElementU.getChildren(xsdAtom.getElement(), XsdU.IMPORT);
        for (final Element child : children) {
            final String namespace = ElementU.getAttribute(child, XsdU.NAMESPACE);
            final String schemaLocation = ElementU.getAttribute(child, XsdU.SCHEMA_LOCATION);
            final URL urlImport = URLCodec.resolve(url, schemaLocation);
            add(namespace, urlImport, xsltX);
        }
    }

    private void doInclude(final URL url, final XsdAtom xsdAtom, final XsltX xsltX) throws IOException {
        final Collection<Element> children = ElementU.getChildren(xsdAtom.getElement(), XsdU.INCLUDE);
        for (final Element child : children) {
            final String schemaLocation = ElementU.getAttribute(child, XsdU.SCHEMA_LOCATION);
            final URL urlInclude = URLCodec.resolve(url, schemaLocation);
            add(null, urlInclude, xsltX);
        }
    }

    private XsltX createTransform() throws IOException {
        return (urlTransform == null) ? null : new XsltX(StreamU.read(urlTransform));
    }
}
