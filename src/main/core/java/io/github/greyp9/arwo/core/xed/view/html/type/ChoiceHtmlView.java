package io.github.greyp9.arwo.core.xed.view.html.type;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceChoice;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

import java.util.Collection;

public class ChoiceHtmlView {
    private final ViewInstanceChoice viewInstance;

    public ChoiceHtmlView(final ViewInstanceChoice viewInstance) {
        this.viewInstance = viewInstance;
    }

    public final void addContentTo(final Element td) {
        final XedCursor cursor = viewInstance.getCursor();
        final XsdBundle bundle = cursor.getXed().getXsdBundle();
        final TypeInstance parentInstance = cursor.getTypeInstance();
        final TypeInstance typeInstance = viewInstance.getTypeInstance();
        final String name = typeInstance.getID(parentInstance);
        final String value = cursor.getValue(typeInstance);
        final Collection<TypeInstance> typeInstances =
                viewInstance.getChoiceInstance().getTypeInstances().getTypeInstances();
        for (final TypeInstance typeInstanceIt : typeInstances) {
            final String nameIt = typeInstanceIt.getName();
            final String labelIt = bundle.getLabel(parentInstance, typeInstanceIt);
            final NameTypeValues attrs = NameTypeValuesU.create(
                    Html.TYPE, Html.RADIO, Html.NAME, name, Html.VALUE, nameIt);
            if (nameIt.equals(value)) {
                attrs.add(NameTypeValue.U.create(Html.CHECKED, Html.CHECKED));
            }
            ElementU.addElement(td, Html.INPUT, null, attrs);
            ElementU.addElement(td, Html.SPAN, labelIt);
        }
    }
}
