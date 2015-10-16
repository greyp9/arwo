package io.github.greyp9.arwo.core.xpath;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class XPather {
    private final Element element;
    private final NamespaceContext context;

    public XPather(final Document document) {
        this(document.getDocumentElement(), null);
    }

    public XPather(final Element element) {
        this(element, null);
    }

    public XPather(final Document document, final NamespaceContext context) {
        this(document.getDocumentElement(), context);
    }

    public XPather(final Element element, final NamespaceContext context) {
        this.element = element;
        this.context = context;
    }

    public final NamespaceContext getContext() {
        return context;
    }

    public final String getText(final String xpath) throws IOException {
        try {
            final XPathExpression expression = getExpression(xpath, context);
            final Object o = (element == null) ? null : expression.evaluate(element, XPathConstants.STRING);
            return ((o instanceof String) ? (String) o : null);
        } catch (XPathExpressionException e) {
            throw new IOException(e);
        }
    }

    public final Element getElement(final String xpath) throws IOException {
        try {
            final XPathExpression expression = getExpression(xpath, context);
            final Object o = (element == null) ? null : expression.evaluate(element, XPathConstants.NODE);
            return ((o instanceof Element) ? (Element) o : null);
        } catch (XPathExpressionException e) {
            throw new IOException(e);
        }
    }

    public final List<Element> getElements(final String xpath) throws IOException {
        try {
            final XPathExpression expression = getExpression(xpath, context);
            final List<Element> elements = new ArrayList<Element>();
            final Object result = (element == null) ? null : expression.evaluate(element, XPathConstants.NODESET);
            if (result instanceof NodeList) {
                final NodeList nodeList = (NodeList) result;
                final int length = nodeList.getLength();
                for (int i = 0; (i < length); i++) {
                    elements.add((Element) nodeList.item(i));
                }
            }
            return elements;
        } catch (XPathExpressionException e) {
            throw new IOException(e);
        }
    }

    public final List<Attr> getAttributes(final String xpath) throws IOException {
        try {
            final XPathExpression expression = getExpression(xpath, context);
            final List<Attr> attributes = new ArrayList<Attr>();
            final Object result = (element == null) ? null : expression.evaluate(element, XPathConstants.NODESET);
            if (result instanceof NodeList) {
                final NodeList nodeList = (NodeList) result;
                final int length = nodeList.getLength();
                for (int i = 0; (i < length); i++) {
                    attributes.add((Attr) nodeList.item(i));
                }
            }
            return attributes;
        } catch (XPathExpressionException e) {
            throw new IOException(e);
        }
    }

    private static XPathExpression getExpression(
            final String expression, final NamespaceContext context) throws IOException {
        try {
            final XPathFactory factory = XPathFactory.newInstance();
            final XPath xpath = factory.newXPath();
            if (context != null) {
                xpath.setNamespaceContext(context);
            }
            return xpath.compile(expression);
        } catch (XPathExpressionException e) {
            throw new IOException(e);
        }
    }
}
