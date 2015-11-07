package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.action.ActionButton;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.type.EnumHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.type.TextHtmlView;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceDrillDown;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceEnum;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceText;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.util.Collection;

public class PropertyStripHtmlView {
    private final XedPropertyPageView view;
    private final XsdBundle xsdBundle;
    private final ActionButtons buttons;

    public PropertyStripHtmlView(final XedPropertyPageView view, final ActionButtons buttons) {
        this.view = view;
        this.xsdBundle = view.getCursor().getXed().getXsdBundle();
        this.buttons = buttons;
    }

    public final void addContentDiv(final Element html) {
        final Element divDialog = ElementU.addElement(html, Html.DIV, null, NameTypeValuesU.create(
                Html.CLASS, Const.DIALOG));
        addContentForm(divDialog);
    }

    public final void addContentForm(final Element html) {
        final String cursorTypeName = view.getCursor().getTypeInstance().getName();
        final Element form = ElementU.addElement(html, Html.FORM, null, NameTypeValuesU.create(
                Html.ACTION, "", Html.ID, String.format("form_%s", cursorTypeName), Html.METHOD, Html.POST));
        addContentTable(form);
    }

    public final void addContentTable(final Element html) {
        // form table (for name / value alignment)
        final Element table = ElementU.addElement(html, Html.TABLE, null, NameTypeValuesU.create(
                Html.CLASS, Const.DIALOG, Html.SUMMARY, Const.DIALOG));
        // form table body
        final Element tbody = ElementU.addElement(table, Html.TBODY, null, NameTypeValuesU.create(
                Html.CLASS, Const.DIALOG));
        final Element tr = ElementU.addElement(tbody, Html.TR);
        // form fields correspond to leaf TypeInstances
        addViewInstances(view.getViewInstances(), tr);
        addActions(tr);
    }

    private void addViewInstances(final Collection<ViewInstance> viewInstances, final Element tr) {
        for (final ViewInstance viewInstance : viewInstances) {
            addViewInstance(viewInstance, tr);
        }
    }

    private void addViewInstance(final ViewInstance viewInstance, final Element tr) {
        final String nameI18n = xsdBundle.getLabel(view.getCursor().getTypeInstance(), viewInstance.getTypeInstance());
        ElementU.addElement(tr, Html.TD, nameI18n);
        addViewInstanceValue(viewInstance, tr);
    }

    private void addViewInstanceValue(final ViewInstance viewInstance, final Element tr) {
        final Element td = ElementU.addElement(tr, Html.TD);
        if (viewInstance instanceof ViewInstanceDrillDown) {
            viewInstance.getClass();
        } else if (viewInstance instanceof ViewInstanceEnum) {
            addViewInstanceValueEnum((ViewInstanceEnum) viewInstance, td);
        } else {
            addViewInstanceValueText((ViewInstanceText) viewInstance, td);
        }
    }

    private void addViewInstanceValueEnum(final ViewInstanceEnum viewInstance, final Element tr) {
        new EnumHtmlView(viewInstance).addContentToStrip(tr);
    }

    private void addViewInstanceValueText(final ViewInstanceText viewInstance, final Element tr) {
        new TextHtmlView(viewInstance).addContentToStrip(tr);
    }

    private void addActions(final Element tr) {
        final Element td = ElementU.addElement(tr, Html.TD);
        for (final ActionButton button : buttons.getButtons()) {
            addAction(button, td);
        }
    }

    private void addAction(final ActionButton button, final Element td) {
        final SubmitToken tokenAction = new SubmitToken(button.getSubject(), button.getAction(), button.getObject());
        HtmlU.addButton(td, button.getLabel(), buttons.getSubmitID(), tokenAction.toString(), null, null);
    }

    private static class Const {
        private static final String DIALOG = "dialog";
    }
}
