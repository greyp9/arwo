package io.github.greyp9.arwo.core.xpath;

import io.github.greyp9.arwo.core.xsd.core.XsdU;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

public final class XPathContextFactory {

    private XPathContextFactory() {
    }

    public static XPathContext create(final Document document) {
        return ((document == null) ? null : create(document.getDocumentElement()));
    }

    public static XPathContext create(final Element element) {
        final XPathContext context = new XPathContext();
        if (element != null) {
            update(context, element);
        }
        return context;
    }

    public static XPathContext update(final XPathContext context, final Element element) {
        final NamedNodeMap attributes = element.getAttributes();
        final int length = attributes.getLength();
        for (int i = 0; (i < length); i++) {
            final Attr attr = (Attr) attributes.item(i);
            final String uri = attr.getNamespaceURI();
            if (XsdU.NS_URI_XML.equals(uri)) {
                context.addMapping(attr.getLocalName(), attr.getValue());
            }
        }
        return context;
    }
}
