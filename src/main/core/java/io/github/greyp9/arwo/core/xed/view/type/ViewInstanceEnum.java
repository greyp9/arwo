package io.github.greyp9.arwo.core.xed.view.type;

import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;

import java.util.Collection;

public class ViewInstanceEnum extends ViewInstance {

    public ViewInstanceEnum(final XedCursor cursor, final TypeInstance typeInstance) {
        super(cursor, typeInstance);
    }

    public final String getValue() {
        final TypeInstance parentInstance = getCursor().getTypeInstance();
        final TypeInstance typeInstance = getTypeInstance();
        final XsdBundle xsdBundle = getCursor().getXed().getXsdBundle();
        final String value = getCursor().getValue(getTypeInstance());
        final StringBuilder buffer = new StringBuilder();
        final Collection<String> enumValues = getTypeInstance().getDataType().getRestrictions().getEnumValues();
        for (final String enumValue : enumValues) {
            final String textEnumValue = xsdBundle.getLabelEnum(parentInstance, typeInstance, enumValue);
            final boolean isSelected = enumValue.equals(value);
            final String textSelected = (isSelected ? "x" : " ");
            buffer.append(String.format("[%s] %s   ", textSelected, textEnumValue));
        }
        return buffer.toString();
    }
}
