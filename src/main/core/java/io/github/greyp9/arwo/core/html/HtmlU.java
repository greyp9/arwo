package io.github.greyp9.arwo.core.html;

import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

public final class HtmlU {

    private HtmlU() {
    }

    @SuppressWarnings("PMD.UseObjectForClearerAPI")
    public static Element addButton(final Element html, final String label, final String name,
                                    final String value, final String htmlClass, final String title) {
        final NameTypeValues attrs = NameTypeValuesU.create(Html.TYPE, Html.SUBMIT,
                Html.CLASS, htmlClass, Html.NAME, name, Html.VALUE, value, Html.TITLE, title);
        return ElementU.addElement(html, Html.BUTTON, label, attrs);
    }
}
