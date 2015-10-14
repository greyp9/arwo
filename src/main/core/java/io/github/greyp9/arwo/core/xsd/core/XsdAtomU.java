package io.github.greyp9.arwo.core.xsd.core;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.xsd.atom.XsdAtom;

import javax.xml.namespace.QName;

public final class XsdAtomU {

    private XsdAtomU() {
    }

    public static QName getQName(final String name, final XsdAtom atom) {
        final int colon = name.indexOf(XsdU.COLON);
        return (colon >= 0) ? getQNameColon(name, colon, atom) :
                new QName(XsdU.NS_URI_XSD, name, XsdU.NS_PREFIX_XSD);
    }

    private static QName getQNameColon(final String name, final int colon, final XsdAtom atom) {
        final String prefix = name.substring(0, colon);
        final String uri = getURI(prefix, atom);
        final String localName = name.substring(colon + XsdU.COLON.length());
        return new QName(uri, localName, prefix);
    }

    private static String getURI(final String prefix, final XsdAtom atomIn) {
        XsdAtom atom = atomIn;
        String uri = XsdU.NS_URI_XSD;
        while (atom.getParent() != null) {
            atom = atom.getParent();
        }
        for (final NameTypeValue nameTypeValue : atom.getNamespaces()) {
            if (prefix.equals(nameTypeValue.getName())) {
                uri = nameTypeValue.getValueS();
                break;
            }
        }
        return uri;
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static String getTargetNamespace(final XsdAtom atom) {
        final String name = atom.getElement().getLocalName();
        if (XsdU.ATTRIBUTE.equals(name)) {
            return null;
        } else if (XsdU.ELEMENT.equals(name)) {
            return getTargetNamespaceElement(atom);
        } else {
            throw new IllegalStateException(atom.getClass().getSimpleName());
        }
    }

    private static String getTargetNamespaceElement(final XsdAtom atomIn) {
        XsdAtom atom = atomIn;
        String uri = null;
        while (atom.getParent() != null) {
            atom = atom.getParent();
        }
        for (final NameTypeValue nameTypeValue : atom.getAttributes()) {
            if (XsdU.TARGET_NAMESPACE.equals(nameTypeValue.getName())) {
                uri = nameTypeValue.getValueS();
                break;
            }
        }
        return uri;
    }
}
