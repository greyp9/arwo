package io.github.greyp9.arwo.core.html;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

@SuppressWarnings("PMD.UseObjectForClearerAPI")
public final class HtmlU {

    private HtmlU() {
    }

    public static Element addButton(final Element html, final String label, final String name, final String value,
                                    final String htmlClass, final String title) {
        final boolean isSubmit = ((name != null) && (value != null));
        final String type = (isSubmit ? Html.SUBMIT : Html.BUTTON);
        final NameTypeValues attrs = NameTypeValuesU.create(Html.TYPE, type,
                Html.CLASS, htmlClass, Html.NAME, name, Html.VALUE, value, Html.TITLE, title);
        if (Html.BUTTON.equals(type)) {
            attrs.add(new NameTypeValue(Html.DISABLED, Html.DISABLED));
        }
        return ElementU.addElement(html, Html.BUTTON, label, attrs);
    }

    public static Element addButton(final Element html, final String label, final String name, final String value,
                                    final String htmlClass, final String title, final String accessKey) {
        final Element button = addButton(html, label, name, value, htmlClass, title);
        ElementU.setAttribute(button, Html.ACCESSKEY, accessKey);
        return button;
    }
}
