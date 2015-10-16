package io.github.greyp9.arwo.core.xpath;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

import javax.xml.XMLConstants;

public class XPathContextFactory {

    public static XPathContext create(Document document) {
        return ((document == null) ? null : create(document.getDocumentElement()));
    }

    public static XPathContext create(Element element) {
        XPathContext context = new XPathContext();
        if (element != null) {
            update(context, element);
        }
        return context;
    }

    public static XPathContext update(XPathContext context, Element element) {
        NamedNodeMap attributes = element.getAttributes();
        int length = attributes.getLength();
        for (int i = 0; (i < length); i++) {
            Attr attr = (Attr) attributes.item(i);
            String uri = attr.getNamespaceURI();
            if (XMLConstants.XMLNS_ATTRIBUTE_NS_URI.equals(uri)) {
                context.addMapping(attr.getLocalName(), attr.getValue());
            }
        }
        return context;
    }
}
