package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.data.NodeType;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.util.Collection;

public class OpFill {

    public final void apply(final XedCursor cursor) {
        final XsdTypes xsdTypes = cursor.getXed().getXsdTypes();
        final DocumentFactory factory = new DocumentFactory(xsdTypes.getTypeDefinitions());
        final QName qname = cursor.getTypeInstance().getQName();
        final Document document = factory.generateEmpty(qname, cursor.getTypeInstance());
        final Xed xedFill = new Xed(document, xsdTypes);
        final XedNav nav = new XedNav(xedFill);
        final XedCursor cursorFill = nav.getRoot();
        apply(cursorFill, cursor);
        final Collection<Element> children = ElementU.getChildren(cursor.getElement());
        for (final Element child : children) {
            final XedCursor cursorChild = nav.find(child, cursor);
            if (cursorChild != null) {
                apply(cursorChild);
            }
        }
    }

    private void apply(final XedCursor cursorFill, final XedCursor cursor) {
        // iterate through child types, inserting content when missing
        final Collection<TypeInstance> typeInstances = cursor.getTypeInstance().getInstances();
        for (final TypeInstance typeInstance : typeInstances) {
            apply(cursorFill, cursor, typeInstance);
        }
    }

    private void apply(final XedCursor cursorFill, final XedCursor cursor, final TypeInstance typeInstance) {
        final NodeType nodeType = typeInstance.getNodeType();
        if (NodeType.attribute.equals(nodeType)) {
            applyAttribute(cursorFill, cursor, typeInstance);
        } else if (NodeType.element.equals(nodeType)) {
            applyElement(cursorFill, cursor, typeInstance);
        }
    }

    private void applyAttribute(final XedCursor cursorFill, final XedCursor cursor, final TypeInstance typeInstance) {
        final Attr attributeFill = ElementU.getAttributeNode(cursorFill.getElement(), typeInstance.getName());
        final Attr attribute = ElementU.getAttributeNode(cursor.getElement(), typeInstance.getName());
        if ((attribute == null) && (attributeFill != null)) {
            ElementU.setAttribute(cursor.getElement(), attributeFill.getName(), attributeFill.getValue());
        }
    }

    private void applyElement(final XedCursor cursorFill, final XedCursor cursor, final TypeInstance typeInstance) {
        final Collection<Element> childrenFill = cursorFill.getChildren(typeInstance);
        final Collection<Element> children = cursor.getChildren(typeInstance);
        if ((children.isEmpty()) && (!childrenFill.isEmpty())) {
            for (final Element childFill : childrenFill) {
                ElementU.importNode(childFill, cursor.getElement());
            }
            new OpOrder().apply(cursor);
        }
    }
}
