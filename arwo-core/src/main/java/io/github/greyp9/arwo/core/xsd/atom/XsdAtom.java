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
    private final NameTypeValues ignored;

    public XsdAtom(final Element element, final XsdAtom parent) throws IOException {
        this.parent = parent;
        this.element = element;
        this.children = new ArrayList<XsdAtom>();
        this.attributes = new NameTypeValues();
        this.namespaces = new NameTypeValues();
        this.directives = new NameTypeValues();
        this.ignored = new NameTypeValues();
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

    public final Collection<XsdAtom> getChildren(final String name) {
        final Collection<XsdAtom> atoms = new ArrayList<XsdAtom>();
        for (final XsdAtom child : children) {
            if (child.getElement().getLocalName().equals(name)) {
                atoms.add(child);
            }
        }
        return atoms;
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

    public final NameTypeValues getIgnored() {
        return ignored;
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

    public final void addIgnored(final NameTypeValue value) {
        ignored.add(value);
    }
}
