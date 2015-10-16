package io.github.greyp9.arwo.core.xpath;

import javax.xml.XMLConstants;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

public class XPathContext implements javax.xml.namespace.NamespaceContext {
    private final Map<String, String> prefixToURI;
    private final Map<String, String> uriToPrefix;

    public Map<String, String> getPrefixToURI() {
        return prefixToURI;
    }

    public XPathContext() {
        this.prefixToURI = new TreeMap<String, String>();
        this.uriToPrefix = new TreeMap<String, String>();
    }

    public int size() {
        return prefixToURI.size();
    }

    public String getNamespaceURI(final String prefix) {
        final String uri = prefixToURI.get(prefix);
        return (uri == null) ? XMLConstants.NULL_NS_URI : uri;
    }

    public String getPrefix(final String uri) {
        final String prefix = uriToPrefix.get(uri);
        return (prefix == null) ? XMLConstants.DEFAULT_NS_PREFIX : prefix;
    }

    public Iterator getPrefixes(final String namespaceURI) {
        throw new UnsupportedOperationException(getClass().getName());
    }

    public void addMapping(final String prefix, final String uri) {
        prefixToURI.put(prefix, uri);
        uriToPrefix.put(uri, prefix);
    }
}
