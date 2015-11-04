package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.action.ActionButton;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.type.DrillDownHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.type.TextHtmlView;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceDrillDown;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceText;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.ArrayList;
import java.util.Collection;

public class PropertyPageHtmlView {
    private final XedPropertyPageView view;
    private final XedRequest request;

    public PropertyPageHtmlView(final XedPropertyPageView view, final XedRequest request) {
        this.view = view;
        this.request = request;
    }

    public final void addContentTo(final Element html) {
        final String cursorTypeName = view.getCursor().getTypeInstance().getName();
        // form wrapper
        final Element divDialog = ElementU.addElement(html, Html.DIV, null, NameTypeValuesU.create(
                Html.CLASS, Const.DIALOG));
        final Element form = ElementU.addElement(divDialog, Html.FORM, null, NameTypeValuesU.create(
                Html.ACTION, "", Html.ID, String.format("form_%s", cursorTypeName), Html.METHOD, Html.POST));
        // form table (for name / value alignment)
        final Element table = ElementU.addElement(form, Html.TABLE, null, NameTypeValuesU.create(
                Html.CLASS, Const.DIALOG, Html.SUMMARY, Const.DIALOG));
        final Element thead = ElementU.addElement(table, Html.THEAD, null, NameTypeValuesU.create(
                Html.CLASS, Const.DIALOG));
        final Element trHead = ElementU.addElement(thead, Html.TR, null, NameTypeValuesU.create(
                Html.CLASS, Const.HEADER));
        final Element th = ElementU.addElement(trHead, Html.TH, null, NameTypeValuesU.create(
                Html.COLSPAN, Integer.toString(2), Html.CLASS, Const.HEADER));
        final String nameI18n = view.getItemNameI18n(view.getCursor().getTypeInstance(), null);
        ElementU.addElement(th, Html.SPAN, nameI18n, NameTypeValuesU.create(Html.CLASS, Const.HEADER));
        // form table body
        final Element tbody = ElementU.addElement(table, Html.TBODY, null, NameTypeValuesU.create(
                Html.CLASS, Const.DIALOG));
        // form fields correspond to leaf TypeInstances
        final Collection<ViewInstance> viewInstances = view.getViewInstances();
        if (viewInstances.isEmpty()) {
            final Element tr = ElementU.addElement(tbody, Html.TR);
            ElementU.addElement(tr, Html.TD, null, NameTypeValuesU.create(Html.CLASS, "empty"));
        } else {
            addViewInstances(viewInstances, tbody);
        }
        addActions(tbody);
    }

    private void addViewInstances(final Collection<ViewInstance> viewInstances, final Element tbody) {
        for (final ViewInstance viewInstance : viewInstances) {
            addViewInstance(viewInstance, tbody);
        }
    }

    private void addViewInstance(final ViewInstance viewInstance, final Element tbody) {
        final Element tr = ElementU.addElement(tbody, Html.TR);
        final String nameI18n = view.getItemNameI18n(
                view.getCursor().getTypeInstance(), viewInstance.getTypeInstance());
        ElementU.addElement(tr, Html.TD, nameI18n, NameTypeValuesU.create(Html.CLASS, "attr-name"));
        addViewInstanceValue(viewInstance, tr);
    }

    private void addViewInstanceValue(final ViewInstance viewInstance, final Element tr) {
        final Element td = ElementU.addElement(tr, Html.TD, null, NameTypeValuesU.create(Html.CLASS, "attr-value"));
        if (viewInstance instanceof ViewInstanceDrillDown) {
            addViewInstanceValueDrillDown((ViewInstanceDrillDown) viewInstance, td);
        } else {
            addViewInstanceValueText((ViewInstanceText) viewInstance, td);
        }
    }

    private void addViewInstanceValueDrillDown(final ViewInstanceDrillDown viewInstance, final Element tr) {
        new DrillDownHtmlView(viewInstance).addContentTo(tr);
    }

    private void addViewInstanceValueText(final ViewInstanceText viewInstance, final Element tr) {
        new TextHtmlView(viewInstance).addContentTo(tr);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void addActions(final Element tbody) {
        final String qname = view.getCursor().getTypeInstance().getQName().toString();
        final String submitID = request.getState().getSubmitID();
        final Bundle bundle = view.getCursor().getXed().getBundle();
        // form submit buttons
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.DOCUMENT, qname);
        final ActionButtons buttons = factory.create(null, getCursorActions(view));
        final Element tr = ElementU.addElement(tbody, Html.TR, null, NameTypeValuesU.create(Html.CLASS, Const.FOOTER));
        final Element th = ElementU.addElement(tr, Html.TD, null, NameTypeValuesU.create(
                Html.COLSPAN, Integer.toString(2), Html.CLASS, Const.DIALOG));
        for (final ActionButton button : buttons.getButtons()) {
            final SubmitToken tokenAction = new SubmitToken(
                    button.getSubject(), button.getAction(), button.getObject());
            HtmlU.addButton(th, button.getLabel(), buttons.getSubmitID(), tokenAction.toString(), null, null);
        }
    }

    private static Collection<String> getCursorActions(final XedPropertyPageView view) {
        final XedCursor cursor = view.getCursor();
        final Node node = cursor.getNode();
        final Node parentNode = ((node == null) ? null : node.getParentNode());
        final Collection<String> actions = new ArrayList<String>();
        final boolean actionCreate = (node == null);
        final boolean actionUpdate = (node != null);
        final boolean actionDelete = ((node != null) && (parentNode instanceof Element));  // can't delete root
        final boolean actionClone = ((node != null) && (parentNode instanceof Element));  // can't clone root
        final boolean actionMove = ((node != null) && (parentNode instanceof Element));
        if (actionCreate) {
            actions.add(App.Action.CREATE);
        }
        if (actionUpdate) {
            actions.add(App.Action.UPDATE);
        }
        if (actionDelete) {
            actions.add(App.Action.DELETE);
        }
        if (actionClone) {
            actions.add(App.Action.CLONE);
        }
        if (actionMove) {
            actions.add(App.Action.UP);
            actions.add(App.Action.DOWN);
        }
        return actions;
    }

    private static class Const {
        private static final String DIALOG = "dialog";
        private static final String HEADER = "header";
        private static final String FOOTER = "footer";
    }
}
