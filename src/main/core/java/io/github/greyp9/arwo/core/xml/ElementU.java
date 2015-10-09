package io.github.greyp9.arwo.core.xml;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.Collection;

public final class ElementU {

    private ElementU() {
    }

    public static String getAttribute(final Element element, final String name) {
        return getAttribute(element, name, null);
    }

    public static String getAttribute(final Element element, final String name, final String defaultValue) {
        final boolean isAttribute = ((element != null) && (name != null) && (element.hasAttribute(name)));
        return isAttribute ? element.getAttribute(name) : defaultValue;
    }

    public static Collection<Element> getChildren(final Element element) {
        final Collection<Element> children = new ArrayList<Element>();
        if (element != null) {
            final NodeList childNodes = element.getChildNodes();
            final int length = childNodes.getLength();
            for (int i = 0; (i < length); ++i) {
                final Node item = childNodes.item(i);
                if (item instanceof Element) {
                    children.add((Element) item);
                }
            }
        }
        return children;
    }

    public static Collection<Element> getChildren(final Element element, final String name) {
        return getChildren(element, name, element.getNamespaceURI());
    }

    public static Collection<Element> getChildren(final Element element, final String name, final String uri) {
        final Collection<Element> children = new ArrayList<Element>();
        final NodeList nodeList = element.getElementsByTagNameNS(uri, name);
        final int length = nodeList.getLength();
        for (int i = 0; (i < length); ++i) {
            final Node item = nodeList.item(i);
            // filter out descendant nodes that are not child nodes
            final Node parentNode = item.getParentNode();
            if (parentNode.equals(element)) {
                children.add((Element) item);
            }
        }
        return children;
    }

    public static Collection<Attr> getAttributes(final Element element) {
        final Collection<Attr> attrs = new ArrayList<Attr>();
        if (element != null) {
            final NamedNodeMap attributes = element.getAttributes();
            final int length = attributes.getLength();
            for (int i = 0; (i < length); ++i) {
                final Node item = attributes.item(i);
                attrs.add((Attr) item);
            }
        }
        return attrs;
    }
}
