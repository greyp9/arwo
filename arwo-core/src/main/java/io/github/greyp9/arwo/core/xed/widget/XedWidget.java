package io.github.greyp9.arwo.core.xed.widget;

import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.action.XedAction;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.util.Locale;

public class XedWidget {
    private final XedAction xedAction;

    public XedWidget(final QName qname, final XedFactory factory, final Locale locale) throws IOException {
        this.xedAction = new XedAction(qname, factory, locale);
    }

    public final XedAction getXedAction() {
        return xedAction;
    }

    public final void applyFrom(final NameTypeValues nameTypeValues) throws IOException {
        xedAction.update(nameTypeValues);
    }

    public final String getValue(final String xpath) throws IOException {
        return xedAction.getXed().getXPather().getText(xpath);
    }

    public final boolean getValueBoolean(final String attribute) throws IOException {
        return Boolean.parseBoolean(getValue(attribute));
    }

    public final int getValueInt(final String attribute) throws IOException {
        String value = getValue(attribute);
        return Integer.parseInt(value);
    }

    public final void addPropertyStripTo(
            final Element html, final String submitID) throws IOException {
        xedAction.addPropertyStripTo(html, submitID);
    }
}
