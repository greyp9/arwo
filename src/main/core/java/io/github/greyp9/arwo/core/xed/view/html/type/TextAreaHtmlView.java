package io.github.greyp9.arwo.core.xed.view.html.type;

import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceTextArea;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import org.w3c.dom.Element;

public class TextAreaHtmlView {
    private final ViewInstanceTextArea viewInstance;

    public TextAreaHtmlView(final ViewInstanceTextArea viewInstance) {
        this.viewInstance = viewInstance;
    }

    public final void addContentTo(final Element td) {
        final XedCursor cursor = viewInstance.getCursor();
        final TypeInstance parentInstance = viewInstance.getCursor().getTypeInstance();
        final TypeInstance typeInstance = viewInstance.getTypeInstance();
        final String name = typeInstance.getID(parentInstance);
        final String value = cursor.getValue(typeInstance);
        final String valueSafe = (Value.isEmpty(value) ? "\n" : value);  // empty textarea issues in clients
        final String rows = typeInstance.getDirective(Html.ROWS);
        final String columns = typeInstance.getDirective(Html.COLS);
        final NameTypeValues attrs = NameTypeValuesU.create(Html.NAME, name, Html.ROWS, rows, Html.COLS, columns);
        ElementU.addElement(td, Html.TEXTAREA, valueSafe, attrs);
    }
}
