package io.github.greyp9.arwo.core.xml;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
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

    public static String getAttributeNS(final Element element, final String name, final String uri) {
        String value = null;
        if ((element != null) && (name != null) && (element.hasAttributeNS(uri, name))) {
            value = element.getAttributeNS(uri, name);
        }
        return value;
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

    public static void setAttribute(final Element element, final String name, final Object value) {
        if ((element != null) && (name != null)) {
            if (value == null) {
                element.removeAttribute(name);
            } else {
                element.setAttributeNS(null, name, value.toString());
            }
        }
    }

    public static Element addElementNS(final Element parent, final String name, final String uri) {
        return addElementBeforeNS(parent, name, uri, null);
    }

    public static Element addElementBeforeNS(
            final Element parent, final String name, final String uri, final Node before) {
        final boolean isNull = (parent == null) || (name == null);
        return (isNull ? null : addElementBeforeNSNN(parent, name, uri, before));
    }

    private static Element addElementBeforeNSNN(
            final Element parent, final String name, final String uri, final Node before) {
        final Document document = parent.getOwnerDocument();
        return addElement(parent, createElement(document, name, uri), before);
    }

    private static Element createElement(final Document document, final String name, final String namespace) {
        return document.createElementNS(namespace, name);
    }

    public static Element addElement(final Element parent, final Element child, final Node before) {
        if ((parent != null) && (child != null)) {
            if (before == null) {
                parent.appendChild(child);
            } else {
                parent.insertBefore(child, before);
            }
        }
        return child;
    }

    public static void setTextContent(final Element element, final Object text) {
        if ((element != null) && (text != null)) {
            element.setTextContent(text.toString());
        }
    }
}
