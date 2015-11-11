package io.github.greyp9.arwo.core.xed.view.html.type;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceBoolean;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

public class BooleanHtmlView {
    private final ViewInstanceBoolean viewInstance;

    public BooleanHtmlView(final ViewInstanceBoolean viewInstance) {
        this.viewInstance = viewInstance;
    }

    public final void addContentTo(final Element td) {
        final XedCursor cursor = viewInstance.getCursor();
        final TypeInstance parentInstance = viewInstance.getCursor().getTypeInstance();
        final TypeInstance typeInstance = viewInstance.getTypeInstance();
        final String name = typeInstance.getID(parentInstance);
        final String value = cursor.getValue(typeInstance);
        final NameTypeValues attrs = NameTypeValuesU.create(Html.NAME, name, Html.TYPE, Html.CHECKBOX);
        if (Boolean.parseBoolean(value)) {
            attrs.add(new NameTypeValue(Html.CHECKED, Html.CHECKED));
        }
        ElementU.addElement(td, Html.INPUT, null, attrs);
    }
}
