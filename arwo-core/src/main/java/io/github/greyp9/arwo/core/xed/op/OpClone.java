package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;

public class OpClone {

/*
    public final Element apply(final Element element) throws IOException {
        // make a deep copy of element node
        final Document documentClone = DocumentU.toDocument(DocumentU.toXml(element));
        final Element elementClone = documentClone.getDocumentElement();
        // transfer ownership of new element node to existing document
        final Document document = element.getOwnerDocument();
        document.adoptNode(elementClone);
        // calculate insert location (end of instances of this type)
        final Element parent = (Element) element.getParentNode();
        final Element next = ElementU.getNextElement(element);
        // insert
        return ElementU.addElement(parent, elementClone, next);
    }
*/

    public final Element apply(final Element element) throws IOException {
        // make a deep copy of element node
        final Document documentClone = DocumentU.toDocument(DocumentU.toXml(element));
        // transfer ownership of new element node to existing document
        final Document document = element.getOwnerDocument();
        final Element elementClone = (Element) document.importNode(documentClone.getDocumentElement(), true);
        // insert
        final Element parent = (Element) element.getParentNode();
        return ElementU.addElement(parent, elementClone, element);
    }
}
