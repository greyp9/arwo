package io.github.greyp9.arwo.core.xpath;

import io.github.greyp9.arwo.core.xsd.core.XsdU;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

public class XPathContext implements javax.xml.namespace.NamespaceContext {
    private final Map<String, String> prefixToURI;
    private final Map<String, String> uriToPrefix;

    public final Map<String, String> getPrefixToURI() {
        return prefixToURI;
    }

    public XPathContext() {
        this.prefixToURI = new TreeMap<>();
        this.uriToPrefix = new TreeMap<>();
    }

    public final int size() {
        return prefixToURI.size();
    }

    public final String getNamespaceURI(final String prefix) {
        final String uri = prefixToURI.get(prefix);
        return (uri == null) ? XsdU.NS_URI_NULL : uri;
    }

    public final String getPrefix(final String uri) {
        final String prefix = uriToPrefix.get(uri);
        return (prefix == null) ? XsdU.NS_PREFIX_NULL : prefix;
    }

    public final Iterator<String> getPrefixes(final String namespaceURI) {
        throw new UnsupportedOperationException(getClass().getName());
    }

    public final void addMapping(final String prefix, final String uri) {
        prefixToURI.put(prefix, uri);
        uriToPrefix.put(uri, prefix);
    }
}
