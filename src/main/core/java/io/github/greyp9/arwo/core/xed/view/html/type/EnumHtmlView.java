package io.github.greyp9.arwo.core.xed.view.html.type;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceEnum;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.util.Collection;

public class EnumHtmlView {
    private final ViewInstanceEnum viewInstance;

    public EnumHtmlView(final ViewInstanceEnum viewInstance) {
        this.viewInstance = viewInstance;
    }

    public final void addContentTo(final Element td) {
        final XedCursor cursor = viewInstance.getCursor();
        final XsdBundle xsdBundle = viewInstance.getCursor().getXed().getXsdBundle();
        final TypeInstance parentInstance = viewInstance.getCursor().getTypeInstance();
        final TypeInstance typeInstance = viewInstance.getTypeInstance();
        final String name = typeInstance.getID(parentInstance);
        final String value = cursor.getValue(typeInstance);
        final Collection<String> enumValues = typeInstance.getDataType().getRestrictions().getEnumValues();
        for (final String enumValue : enumValues) {
            final String labelIt = xsdBundle.getLabelEnum(parentInstance, typeInstance, enumValue);
            final NameTypeValues attrs = NameTypeValuesU.create(
                    Html.TYPE, Html.RADIO, Html.NAME, name, Html.VALUE, enumValue, Html.ACCESSKEY, Html.VALUE_1);
            if (enumValue.equals(value)) {
                attrs.add(NameTypeValue.U.create(Html.CHECKED, Html.CHECKED));
            }
            ElementU.addElement(td, Html.INPUT, null, attrs);
            ElementU.addElement(td, Html.SPAN, labelIt);
        }
    }

    public final void addContentToStrip(final Element td) {
        final XedCursor cursor = viewInstance.getCursor();
        final XsdBundle xsdBundle = viewInstance.getCursor().getXed().getXsdBundle();
        final TypeInstance parentInstance = viewInstance.getCursor().getTypeInstance();
        final TypeInstance typeInstance = viewInstance.getTypeInstance();
        final String name = typeInstance.getID(parentInstance);
        final String value = cursor.getValue(typeInstance);
        final Collection<String> enumValues = typeInstance.getDataType().getRestrictions().getEnumValues();
        for (final String enumValue : enumValues) {
            final String labelIt = xsdBundle.getLabelEnumCompact(parentInstance, typeInstance, enumValue);
            final NameTypeValues attrs = NameTypeValuesU.create(
                    Html.TYPE, Html.RADIO, Html.NAME, name, Html.VALUE, enumValue);
            if (enumValue.equals(value)) {
                attrs.add(NameTypeValue.U.create(Html.CHECKED, Html.CHECKED));
            }
            ElementU.addElement(td, Html.INPUT, null, attrs);
            ElementU.addElement(td, Html.SPAN, labelIt);
        }
    }
}
