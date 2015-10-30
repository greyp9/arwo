package io.github.greyp9.arwo.core.xsd.atom;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collection;
import java.util.logging.Logger;

public class XsdAtomFactory {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final XsdAtomEnumeration atoms;

    public XsdAtomFactory() {
        this.atoms = new XsdAtomEnumeration();
    }

    public final XsdAtom create(final Element element, final XsdAtom parentAtom) throws IOException {
        final XsdAtom atom = new XsdAtom(element, parentAtom);
        final Collection<Element> children = ElementU.getChildren(element);
        for (final Element child : children) {
            loadChild(child, atom);
        }
        final Collection<Attr> attrs = ElementU.getAttributes(element);
        for (final Attr attr : attrs) {
            loadAttribute(attr, atom);
        }
        return atom;
    }

    private void loadChild(final Element element, final XsdAtom parentAtom) throws IOException {
        final Element parent = parentAtom.getElement();
        if (atoms.isChild(parent, element)) {
            parentAtom.addChild(create(element, parentAtom));
        } else if (atoms.isChildIgnore(parent, element)) {
            atoms.getClass();
        } else {
            logger.warning(String.format("[%s]/[%s]", QNameU.getQName(parent), QNameU.getQName(element)));
            //throw new IOException(String.format("[%s]/@[%s]", QNameU.getQName(element), QNameU.getQName(element)));
        }
    }

    private void loadAttribute(final Attr attr, final XsdAtom parentAtom) throws IOException {
        final Element parent = parentAtom.getElement();
        final String uri = ((attr.getNamespaceURI() == null) ? parent.getNamespaceURI() : attr.getNamespaceURI());
        final NameTypeValue nameTypeValue = new NameTypeValue(attr.getLocalName(), uri, attr.getValue());
        if (XsdU.NS_URI_XML.equals(nameTypeValue.getType())) {
            parentAtom.addNamespace(new NameTypeValue(nameTypeValue.getName(), nameTypeValue.getValueS()));
        } else if ((XsdU.NS_URI_XSD.equals(nameTypeValue.getType())) && (atoms.isAttr(parent, attr))) {
            parentAtom.addAttribute(nameTypeValue);
        } else if ((XsdU.NS_URI_XSD.equals(nameTypeValue.getType())) && (atoms.isAttrIgnore(parent, attr))) {
            atoms.getClass();
        } else if (XsdU.NS_URI_XSD.equals(nameTypeValue.getType())) {
            logger.warning(String.format("[%s]/[@%s]", QNameU.getQName(parent), QNameU.getQName(attr)));
            //throw new IOException(String.format("[%s]/@[%s]", QNameU.getQName(element), QNameU.getQName(attr)));
        } else if (XsdU.Xed.NS_URI_XED.equals(nameTypeValue.getType())) {
            parentAtom.addDirective(nameTypeValue);
        } else {
            parentAtom.addIgnored(nameTypeValue);
        }
    }
}
