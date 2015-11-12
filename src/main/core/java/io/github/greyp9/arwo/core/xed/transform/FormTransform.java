package io.github.greyp9.arwo.core.xed.transform;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xsd.core.XsdTypeU;
import io.github.greyp9.arwo.core.xsd.data.DataType;
import io.github.greyp9.arwo.core.xsd.instance.ChoiceTypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceX;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;

import javax.xml.namespace.QName;
import java.util.Collection;

public class FormTransform {

    public final ValueInstance transform(final ValueInstance valueInstanceIn) {
        final TypeInstance parentInstance = valueInstanceIn.getTypeInstance();
        final ValueInstance valueInstance = new ValueInstance(parentInstance);
        final NameTypeValues nameTypeValues = new NameTypeValues(valueInstanceIn.getNameTypeValues());
        final Collection<TypeInstance> typeInstances = new TypeInstanceX(parentInstance).getPageInstances();
        for (final TypeInstance childInstance : typeInstances) {
            final String nameFull = childInstance.getID(parentInstance);
            final NameTypeValue nameTypeValue = nameTypeValues.getNameValue(nameFull);
            final DataType dataType = childInstance.getDataType();
            final QName qname = (dataType == null) ? null : dataType.getQName();
            if (XsdTypeU.Const.BOOLEAN.equals(qname)) {
                valueInstance.add(transformBoolean(nameFull, nameTypeValue));
            } else if (childInstance instanceof ChoiceTypeInstance) {
                valueInstance.add(transformChoice(parentInstance, (ChoiceTypeInstance) childInstance, nameTypeValue));
            } else if (nameTypeValue != null) {
                valueInstance.add(nameTypeValue);
            }
            nameTypeValues.remove(nameTypeValue);
        }
        for (final NameTypeValue nameTypeValue : nameTypeValues) {
            valueInstance.add(nameTypeValue);
        }
        return valueInstance;
    }

    private NameTypeValue transformBoolean(final String nameIn, final NameTypeValue nameTypeValueIn) {
        NameTypeValue nameTypeValue = nameTypeValueIn;
        if (nameTypeValueIn == null) {
            nameTypeValue = new NameTypeValue(nameIn, Boolean.FALSE.toString());
        } else if (Html.ON.equals(nameTypeValueIn.getValue())) {
            nameTypeValue = new NameTypeValue(nameIn, Boolean.TRUE.toString());
        }
        return nameTypeValue;
    }

    private NameTypeValue transformChoice(final TypeInstance parentInstance, final ChoiceTypeInstance choiceInstance,
                                          final NameTypeValue nameTypeValueIn) {
        NameTypeValue nameTypeValue = nameTypeValueIn;
        if (nameTypeValueIn == null) {
            final String idInstance = choiceInstance.getID(parentInstance);
            final TypeInstance typeInstanceFirst = choiceInstance.getInstances().iterator().next();
            nameTypeValue = new NameTypeValue(idInstance, typeInstanceFirst.getName());
        }
        return nameTypeValue;
    }
}
