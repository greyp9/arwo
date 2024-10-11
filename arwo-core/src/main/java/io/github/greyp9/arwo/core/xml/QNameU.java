package io.github.greyp9.arwo.core.xml;

import io.github.greyp9.arwo.core.xsd.core.XsdU;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class QNameU {

    private QNameU() {
    }

    public static String toStringColon(final QName qname) {
        return String.format("%s:%s", qname.getPrefix(), qname.getLocalPart());
    }

    public static QName getQNameColon(final String qname) {
        final Pattern pattern = Pattern.compile("(.*):(.+)");
        final Matcher matcher = pattern.matcher(qname);
        return ((matcher.matches())
                ? new QName(XsdU.NS_URI_NULL, matcher.group(2), matcher.group(1))
                : new QName(XsdU.NS_URI_NULL, matcher.group(2), XsdU.NS_PREFIX_NULL));
    }

    public static QName getQName(final String qNameToString) {
        final Pattern pattern = Pattern.compile("\\{(.*)\\}(.+)");
        final Matcher matcher = pattern.matcher(qNameToString);
        return ((matcher.matches())
                ? new QName(matcher.group(1), matcher.group(2))
                : new QName(XsdU.NS_URI_NULL, qNameToString));
    }

    public static QName getQName(final Element element) {
        return getQName(element.getNamespaceURI(), element.getLocalName(), element.getPrefix());
    }

    public static QName getQName(final Attr attr) {
        return getQName(attr.getNamespaceURI(), attr.getLocalName(), attr.getPrefix());
    }

    public static QName getQName(final String uri, final String name) {
        return getQName(uri, name, null);
    }

    public static QName getQName(final String uri, final String name, final String prefix) {
        return ((prefix == null) ? new QName(uri, name) : new QName(uri, name, prefix));
    }
}
