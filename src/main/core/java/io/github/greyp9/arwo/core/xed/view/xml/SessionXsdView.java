package io.github.greyp9.arwo.core.xed.view.xml;

import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.session.XedSession;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.atom.SchemaAtom;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.source.SchemaCollection;
import io.github.greyp9.arwo.core.xsd.structure.TypeDefinitions;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Collection;

public class SessionXsdView {
    private final XedSession session;

    public SessionXsdView(final XedSession session) {
        this.session = session;
    }

    public final HttpResponse doGetXSD() throws IOException {
        final TypeDefinitions typeDefinitions = session.getXed().getXsdTypes().getTypeDefinitions();
        final SchemaCollection schemaCollection = typeDefinitions.getTypeComponents().getSchemaCollection();
        final Document documentSchemaSet = DocumentU.createDocument(XsdU.SCHEMAS, null);
        final Collection<SchemaAtom> schemaAtoms = schemaCollection.getSchemas().values();
        for (final SchemaAtom schemaAtom : schemaAtoms) {
            final byte[] xmlIt = DocumentU.toXml(schemaAtom.getAtom().getElement());
            final Document documentIt = DocumentU.toDocument(xmlIt);
            final Element elementIt = documentIt.getDocumentElement();
            documentSchemaSet.adoptNode(elementIt);
            ElementU.addElement(documentSchemaSet.getDocumentElement(), elementIt, null);
        }
        final byte[] xsd = DocumentU.toXml(documentSchemaSet);
        final NameTypeValues headers = new NameTypeValues(
                new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_XML_UTF8),
                new NameTypeValue(Http.Header.CONTENT_LENGTH, xsd.length));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(xsd));
    }
}
