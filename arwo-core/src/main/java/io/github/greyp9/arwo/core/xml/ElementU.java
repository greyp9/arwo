package io.github.greyp9.arwo.core.xml;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.namespace.QName;
import java.util.ArrayList;
import java.util.Collection;

@SuppressWarnings({ "PMD.GodClass", "PMD.TooManyMethods" })
public final class ElementU {

    private ElementU() {
    }

    public static String getTextContent(final Element element) {
        return ((element == null) ? null : element.getTextContent());
    }

    public static Attr getAttributeNode(final Element element, final String name) {
        final boolean isAttribute = ((element != null) && (name != null) && (element.hasAttribute(name)));
        return isAttribute ? element.getAttributeNode(name) : null;
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

    public static Element getChild(final Element element, final QName qname) {
        return getChild(element, qname.getLocalPart(), qname.getNamespaceURI());
    }

    public static Element getChild(final Element element, final String name) {
        final String uri = ((element == null) ? null : element.getNamespaceURI());
        return getChild(element, name, uri);
    }

    public static Element getChild(final Element element, final String name, final String uri) {
        return (element == null) ? null : getChildNN(element, name, uri);
    }

    private static Element getChildNN(final Element element, final String name, final String uri) {
        final NodeList elements = element.getElementsByTagNameNS(uri, name);
        return (Element) ((elements.getLength() == 0) ? null : elements.item(0));
    }

    public static Collection<Element> getChildren(final Node node) {
        return ((node instanceof Element) ? getChildren((Element) node) : Const.EMPTY_LIST_E);
    }

    public static Collection<Attr> getAttributes(final Node node) {
        return ((node instanceof Element) ? getAttributes((Element) node) : Const.EMPTY_LIST_A);
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

    public static void setAttributes(final Element element, final NameTypeValues attrs) {
        for (final NameTypeValue attr : attrs) {
            setAttribute(element, attr.getName(), attr.getValue());
        }
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

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Node detach(final Node node) {
        if (node instanceof Element) {
            return detach((Element) node);
        } else if (node instanceof Attr) {
            return detach((Attr) node);
        } else {
            return node;
        }
    }

    public static Element detach(final Element element) {
        if ((element != null) && (element.getParentNode() != null)) {
            element.getParentNode().removeChild(element);
        }
        return element;
    }

    public static Attr detach(final Attr attr) {
        if ((attr != null) && (attr.getOwnerElement() != null)) {
            attr.getOwnerElement().removeAttributeNode(attr);
        }
        return attr;
    }

    public static boolean isEmpty(final Node node) {
        return ((node != null) && (node.getChildNodes().getLength() == 0));
    }

    public static Node detachOnEmpty(final Node node) {
        if (isEmpty(node)) {
            detach(node);
        }
        return node;
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

    public static Element addElement(final Element parent, final String name) {
        return addElementNS(parent, name, parent.getNamespaceURI());
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

    public static void setTextContentNullable(final Element element, final Object text) {
        if (element != null) {
            if (text == null) {
                detach(element);
            } else {
                element.setTextContent(text.toString());
            }
        }
    }

    public static void addElementNullable(final Element parent, final String name, final Object text) {
        final Element child = addElement(parent, name);
        setTextContentNullable(child, text);
    }

    public static Element addElement(final Element parent, final String name, final Object text) {
        return addElement(parent, name, text, NameTypeValuesU.create());
    }

    public static Element addElement(
            final Element parent, final String name, final Object text, final NameTypeValues attrs) {
        final Element child = addElement(parent, name);
        setTextContent(child, text);
        setAttributes(child, attrs);
        return child;
    }

    public static Element addElementFirst(
            final Element parent, final String name, final Object text, final NameTypeValues attrs) {
        final Element child = addElementBeforeNS(parent, name, parent.getNamespaceURI(), parent.getFirstChild());
        setTextContent(child, text);
        setAttributes(child, attrs);
        return child;
    }

    public static Element addElement(final Element parent, final String name, final Object text, final Node before) {
        final Element child = addElementBeforeNS(parent, name, parent.getNamespaceURI(), before);
        setTextContent(child, text);
        return child;
    }

    public static void insertBefore(final Node toInsert, final Element insertBefore) {
        if ((toInsert != null) && (insertBefore != null)) {
            final Node toInsertNew = insertBefore.getOwnerDocument().importNode(toInsert, true);
            final Node parentNode = insertBefore.getParentNode();
            parentNode.insertBefore(toInsertNew, insertBefore);
        }
    }

    public static void importNode(final Node child, final Node parentNode) {
        final Node toInsertNew = parentNode.getOwnerDocument().importNode(child, true);
        parentNode.appendChild(toInsertNew);
    }

    public static Element getParent(final Element element) {
        final Node parentNode = (element == null) ? null : element.getParentNode();
        return Value.as(parentNode, Element.class);
    }

    public static Element getPreviousElement(final Element element) {
        Node previous = ((element == null) ? null : element.getPreviousSibling());
        while ((previous != null) && (!(previous instanceof Element))) {
            previous = previous.getPreviousSibling();
        }
        return (Element) previous;
    }

    public static Element getNextElement(final Element element) {
        Node next = ((element == null) ? null : element.getNextSibling());
        while ((next != null) && (!(next instanceof Element))) {
            next = next.getNextSibling();
        }
        return (Element) next;
    }

    private static class Const {
        private static final Collection<Element> EMPTY_LIST_E = new ArrayList<Element>();
        private static final Collection<Attr> EMPTY_LIST_A = new ArrayList<Attr>();
    }
}
