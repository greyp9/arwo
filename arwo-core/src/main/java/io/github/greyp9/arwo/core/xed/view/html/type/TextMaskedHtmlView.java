package io.github.greyp9.arwo.core.xed.view.html.type;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceTextMasked;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

public class TextMaskedHtmlView {
    private final ViewInstanceTextMasked viewInstance;

    public TextMaskedHtmlView(final ViewInstanceTextMasked viewInstance) {
        this.viewInstance = viewInstance;
    }

    public final void addContentTo(final Element td) {
        final XedCursor cursor = viewInstance.getCursor();
        final XsdBundle bundle = cursor.getXed().getXsdBundle();
        final TypeInstance parentInstance = viewInstance.getCursor().getTypeInstance();
        final TypeInstance typeInstance = viewInstance.getTypeInstance();
        final String name = typeInstance.getID(parentInstance);
        final String value = Html.MASK;  // cursor.getValue(typeInstance);
        final String title = bundle.getDetail(cursor.getTypeInstance(), viewInstance.getTypeInstance());
        final String size = Integer.toString(Const.WIDTH_INPUT_TEXT);
        final NameTypeValues attrs = NameTypeValuesU.create(Html.NAME, name, Html.TYPE, Html.PASSWORD,
                Html.VALUE, value, Html.SIZE, size, Html.TITLE, title, Html.ACCESSKEY, Html.VALUE_1);
        ElementU.addElement(td, Html.INPUT, null, attrs);
    }

    public final void addContentToStrip(final Element td) {
        //final XedCursor cursor = viewInstance.getCursor();
        final TypeInstance parentInstance = viewInstance.getCursor().getTypeInstance();
        final TypeInstance typeInstance = viewInstance.getTypeInstance();
        final String name = typeInstance.getID(parentInstance);
        final String value = Html.MASK;  // cursor.getValue(typeInstance);
        final NameTypeValues attrs = NameTypeValuesU.create(Html.NAME, name, Html.TYPE, Html.PASSWORD,
                Html.VALUE, value);
        ElementU.addElement(td, Html.INPUT, null, attrs);
    }

    private static class Const {
        private static final int WIDTH_INPUT_TEXT = 64;
    }
}
