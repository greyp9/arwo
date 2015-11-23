package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.ConcreteTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.util.Collection;
import java.util.Iterator;

public class OpOrder {

    public final void apply(final XedCursor cursor) {
        // detach
        final Element parent = cursor.getElement();
        final Collection<Element> children = ElementU.getChildren(parent);
        for (final Element child : children) {
            ElementU.detach(child);
        }
        // re-attach, in order
        final Collection<TypeInstance> typeInstances = cursor.getTypeInstance().getInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            applyTypeInstance(parent, children, typeInstanceIt);
        }
        // replace any non-schema content
        for (final Element child : children) {
            ElementU.addElement(parent, child, null);
        }
    }

    private void applyTypeInstance(
            final Element parent, final Collection<Element> children, final TypeInstance typeInstance) {
        if (typeInstance instanceof ChoiceTypeInstance) {
            applyChoiceInstance(parent, children, (ChoiceTypeInstance) typeInstance);
        } else if (typeInstance instanceof ConcreteTypeInstance) {
            applyConcreteInstance(parent, children, (ConcreteTypeInstance) typeInstance);
        }
    }

    private void applyChoiceInstance(
            final Element parent, final Collection<Element> children, final ChoiceTypeInstance choiceInstance) {
        final Collection<TypeInstance> typeInstances = choiceInstance.getInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            applyTypeInstance(parent, children, typeInstanceIt);
        }
    }

    private void applyConcreteInstance(
            final Element parent, final Collection<Element> children, final ConcreteTypeInstance concreteInstance) {
        for (final Iterator<Element> iterator = children.iterator(); iterator.hasNext(); iterator.getClass()) {
            final Element child = iterator.next();
            if (child.getLocalName().equals(concreteInstance.getName())) {
                ElementU.addElement(parent, child, null);
                iterator.remove();
            }
        }
    }
}
