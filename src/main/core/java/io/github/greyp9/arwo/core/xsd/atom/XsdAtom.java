package io.github.greyp9.arwo.core.xsd.atom;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

public class XsdAtom {
    private final Element element;
    private final XsdAtom parent;
    private final Collection<XsdAtom> children;
    private final NameTypeValues attributes;
    private final NameTypeValues namespaces;
    private final NameTypeValues directives;

/*
    public XsdAtom(Element element) throws IOException {
        this(element, null);
    }
*/

    public XsdAtom(final Element element, final XsdAtom parent) throws IOException {
        this.parent = parent;
        this.element = element;
        this.children = new ArrayList<XsdAtom>();
        this.attributes = new NameTypeValues();
        this.namespaces = new NameTypeValues();
        this.directives = new NameTypeValues();
    }

    public final Element getElement() {
        return element;
    }

    public final XsdAtom getParent() {
        return parent;
    }

    public final Collection<XsdAtom> getChildren() {
        return children;
    }

    public final NameTypeValues getAttributes() {
        return attributes;
    }

    public final NameTypeValues getNamespaces() {
        return namespaces;
    }

    public final NameTypeValues getDirectives() {
        return directives;
    }

    public final void addChild(final XsdAtom atom) {
        children.add(atom);
    }

    public final void addAttribute(final NameTypeValue value) {
        attributes.add(value);
    }

    public final void addNamespace(final NameTypeValue value) {
        namespaces.add(value);
    }

    public final void addDirective(final NameTypeValue value) {
        directives.add(value);
    }
}
