package io.github.greyp9.arwo.core.xed.op;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.transform.TransformContext;
import io.github.greyp9.arwo.core.xed.transform.ValueInstanceTransform;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.core.XsdU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.data.DataTypeU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactoryU;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.security.Key;
import java.util.Collection;

public class OpCreate {
    private final Key key;
    private final Xed xed;

    public OpCreate(final Key key, final Xed xed) {
        this.key = key;
        this.xed = xed;
    }

    public final Element apply(final Element element, final ValueInstance valueInstanceIn) throws IOException {
        final TypeInstance typeInstance = valueInstanceIn.getTypeInstance();
        final Element create = ElementU.addElementBeforeNS(
                element, typeInstance.getName(), typeInstance.getURI(), null);
        final XPather xpather = new XPather(create, xed.getXPather().getContext());
        final TransformContext context = new TransformContext(key, xpather);
        final ValueInstance valueInstance = new ValueInstanceTransform(context).transform(valueInstanceIn);
        final NameTypeValues nameTypeValues = valueInstance.getNameTypeValues();
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(typeInstance).getPageInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            apply(create, typeInstance, typeInstanceIt, nameTypeValues);
        }
        new OpOrder().apply(new XedNav(xed).find(element));  // reorder children to match xsd
        return create;
    }

    private void apply(final Element create, final TypeInstance parentInstance,
                       final TypeInstance typeInstance, final NameTypeValues nameTypeValues) {
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            apply(create, parentInstance, typeInstance, nameTypeValue);
        }
    }

    private void apply(final Element create, final TypeInstance parentInstance,
                       final TypeInstance typeInstance, final NameTypeValue nameTypeValue) {
        final boolean match = typeInstance.getID(parentInstance).equals(nameTypeValue.getName());
        if (match) {
            final NameTypeValue nameTypeValueIt = new NameTypeValue(typeInstance.getName(), nameTypeValue.getValue());
            applyValue(create, typeInstance, nameTypeValueIt);
        }
    }

    private void applyValue(final Element create, final TypeInstance typeInstance, final NameTypeValue nameTypeValue) {
        final TypeInstance.NodeType nodeType = typeInstance.getNodeType();
        if (TypeInstance.NodeType.baseType.equals(nodeType)) {
            applyBaseType(create, nameTypeValue, typeInstance);
        } else if (TypeInstance.NodeType.attribute.equals(nodeType)) {
            applyAttribute(create, nameTypeValue);
        } else if (TypeInstance.NodeType.element.equals(nodeType)) {
            applyElement(create, nameTypeValue);
        } else if (TypeInstance.NodeType.choice.equals(nodeType) && (typeInstance instanceof ChoiceTypeInstance)) {
            applyChoice(create, nameTypeValue, (ChoiceTypeInstance) typeInstance);
        }
    }

    private void applyBaseType(
            final Element create, final NameTypeValue nameTypeValue, final TypeInstance typeInstance) {
        final DataType baseTypeRoot = DataTypeU.getRootBaseType(typeInstance.getDataType());
        if (XsdU.NS_URI_XSD.equals(baseTypeRoot.getQName().getNamespaceURI())) {
            ElementU.setTextContentNullable(create, nameTypeValue.getValue());
        }
    }

    private void applyAttribute(final Element create, final NameTypeValue nameTypeValue) {
        ElementU.setAttribute(create, nameTypeValue.getName(), nameTypeValue.getValue());
    }

    private void applyElement(final Element create, final NameTypeValue nameTypeValue) {
        ElementU.addElement(create, nameTypeValue.getName(), nameTypeValue.getValue(), NameTypeValuesU.create());
    }

    private void applyChoice(
            final Element create, final NameTypeValue nameTypeValue, final ChoiceTypeInstance choiceInstance) {
        final String value = nameTypeValue.getValueS();
        final TypeInstance typeInstance = choiceInstance.getInstance(value);
        final Document document = DocumentFactoryU.generateDocument(xed.getXsdTypes(), typeInstance, true);
        ElementU.importNode(document.getDocumentElement(), create);
    }
}
