package io.github.greyp9.arwo.core.xed.session;

import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpFill;
import io.github.greyp9.arwo.core.xed.trigger.XedTrigger;
import io.github.greyp9.arwo.core.xed.trigger.XedTriggerFactory;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Date;

public class XedSessionFactory {
    private final XedEntry entry;

    public XedSessionFactory(final XedEntry entry) {
        this.entry = entry;
    }

    public final XedSession create(final String qnameColon) throws IOException {
        final XedTrigger trigger = new XedTriggerFactory().createTrigger(entry.getTrigger());
        // normalize paths
        final String xmlPath = SystemU.resolve(entry.getXmlPath());
        final String xsdPath = resolveInternal(SystemU.resolve(entry.getXsdPath()));
        final String xsltPath = resolveInternal(SystemU.resolve(entry.getXsltPath()));
        // load types
        final File fileXml = FileU.toFile(xmlPath);
        final URL urlXsd = URLCodec.toURL(xsdPath);
        final URL urlXslt = URLCodec.toURL(Value.isEmpty(xsltPath) ? null : xsltPath);
        final XsdTypes xsdTypes = new XsdTypes(urlXsd, null, urlXslt);
        // load / fabricate document
        final boolean existsFile = ((fileXml != null) && (fileXml.exists()) && (fileXml.isFile()));
        final QName qname = xsdTypes.getQName(qnameColon);
        final Document document = (existsFile ? loadDocument(fileXml) : fabricateDocument(xsdTypes, qname));
        // fabricate session
        final Xed xed = new Xed(document, xsdTypes);
        // populate any missing content
        new OpFill().apply(new XedNav(xed).getRoot());
        // wrap document in editor session
        return new XedSession(entry, xed, fileXml, new Date(), trigger);
    }

    private String resolveInternal(final String path) throws IOException {
        final boolean isInternal = ((path != null) && (path.startsWith("io/github/greyp9/arwo")));  // i18n internal
        return (isInternal ? ResourceU.resolve(path).toExternalForm() : path);
    }

    private Document fabricateDocument(final XsdTypes xsdTypes, final QName qname) throws IOException {
        final DocumentFactory documentFactory = new DocumentFactory(xsdTypes.getTypeDefinitions());
        return documentFactory.generateEmpty(qname);
    }

    private Document loadDocument(final File file) throws IOException {
        return DocumentU.toDocument(StreamU.read(file));
    }
}
