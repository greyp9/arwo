package io.github.greyp9.arwo.core.xed.session;

import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Date;
import java.util.Locale;

public class XedSessionFactory {
    private final XedEntry entry;

    public XedSessionFactory(final XedEntry entry) {
        this.entry = entry;
    }

    public final XedSession create(final QName qname, final Locale locale) throws IOException {
        // normalize paths
        final String xmlPath = SystemU.resolve(entry.getXmlPath());
        final String xsdPath = SystemU.resolve(entry.getXsdPath());
        final String xsltPath = SystemU.resolve(entry.getXsltPath());
        // load types
        final File fileXml = FileU.toFile(xmlPath);
        final URL urlXsd = URLCodec.toURL(xsdPath);
        final URL urlXslt = URLCodec.toURL(Value.isEmpty(xsltPath) ? null : xsltPath);
        final XsdTypes xsdTypes = new XsdTypes(urlXsd, null, urlXslt);
        // load / fabricate document
        final boolean existsFile = ((fileXml != null) && (fileXml.exists()) && (fileXml.isFile()));
        final Document document = (existsFile ? loadDocument(fileXml) : fabricateDocument(xsdTypes, qname));
        // fabricate session
        final Xed xed = new Xed(document, xsdTypes, locale);
        // populate any missing content
        //new XedFill().apply(new XedNav(xed).getRoot());
        // wrap document in editor session
        return new XedSession(entry, xed, fileXml, new Date());
    }

    private Document fabricateDocument(final XsdTypes xsdTypes, final QName qname) throws IOException {
        final DocumentFactory documentFactory = new DocumentFactory(xsdTypes.getTypeDefinitions());
        return documentFactory.generateEmpty(qname);
    }

    private Document loadDocument(final File file) throws IOException {
        return DocumentU.toDocument(StreamU.read(file));
    }
}