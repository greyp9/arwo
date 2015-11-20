package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.action.ActionButton;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.type.BooleanHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.type.ChoiceHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.type.DrillDownHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.type.EnumHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.type.TextAreaHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.type.TextHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.type.TextMaskedHtmlView;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstance;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceBoolean;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceChoice;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceDrillDown;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceEnum;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceText;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceTextArea;
import io.github.greyp9.arwo.core.xed.view.type.ViewInstanceTextMasked;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

@SuppressWarnings({"PMD.ExcessiveImports", "PMD.TooManyMethods"})
public class PropertyPageHtmlView {
    private final XedPropertyPageView view;
    private final XsdBundle xsdBundle;
    private final XedRequest request;

    public PropertyPageHtmlView(final XedPropertyPageView view, final XedRequest request) {
        this.view = view;
        this.xsdBundle = view.getCursor().getXed().getXsdBundle();
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
        final String nameI18n = xsdBundle.getLabel(view.getCursor().getTypeInstance());
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
        final String nameI18n = xsdBundle.getLabel(view.getCursor().getTypeInstance(), viewInstance.getTypeInstance());
        ElementU.addElement(tr, Html.TD, nameI18n, NameTypeValuesU.create(Html.CLASS, "attr-name"));
        addViewInstanceValue(viewInstance, tr);
    }

    private void addViewInstanceValue(final ViewInstance viewInstance, final Element tr) {
        final Element td = ElementU.addElement(tr, Html.TD, null, NameTypeValuesU.create(Html.CLASS, "attr-value"));
        if (viewInstance instanceof ViewInstanceDrillDown) {
            addViewInstanceValueDrillDown((ViewInstanceDrillDown) viewInstance, td);
        } else if (viewInstance instanceof ViewInstanceBoolean) {
            addViewInstanceValueBoolean((ViewInstanceBoolean) viewInstance, td);
        } else if (viewInstance instanceof ViewInstanceEnum) {
            addViewInstanceValueEnum((ViewInstanceEnum) viewInstance, td);
        } else if (viewInstance instanceof ViewInstanceChoice) {
            addViewInstanceValueChoice((ViewInstanceChoice) viewInstance, td);
        } else if (viewInstance instanceof ViewInstanceTextArea) {
            addViewInstanceValueTextArea((ViewInstanceTextArea) viewInstance, td);
        } else if (viewInstance instanceof ViewInstanceTextMasked) {
            addViewInstanceValueTextMasked((ViewInstanceTextMasked) viewInstance, td);
        } else {
            addViewInstanceValueText((ViewInstanceText) viewInstance, td);
        }
    }

    private void addViewInstanceValueDrillDown(final ViewInstanceDrillDown viewInstance, final Element tr) {
        new DrillDownHtmlView(viewInstance).addContentTo(tr);
    }

    private void addViewInstanceValueBoolean(final ViewInstanceBoolean viewInstance, final Element tr) {
        new BooleanHtmlView(viewInstance).addContentTo(tr);
    }

    private void addViewInstanceValueEnum(final ViewInstanceEnum viewInstance, final Element tr) {
        new EnumHtmlView(viewInstance).addContentTo(tr);
    }

    private void addViewInstanceValueChoice(final ViewInstanceChoice viewInstance, final Element tr) {
        new ChoiceHtmlView(viewInstance).addContentTo(tr);
    }

    private void addViewInstanceValueTextArea(final ViewInstanceTextArea viewInstance, final Element tr) {
        new TextAreaHtmlView(viewInstance).addContentTo(tr);
    }

    private void addViewInstanceValueTextMasked(final ViewInstanceTextMasked viewInstance, final Element tr) {
        new TextMaskedHtmlView(viewInstance).addContentTo(tr);
    }

    private void addViewInstanceValueText(final ViewInstanceText viewInstance, final Element tr) {
        new TextHtmlView(viewInstance).addContentTo(tr);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void addActions(final Element tbody) {
        final String qname = view.getCursor().getTypeInstance().getQName().toString();
        final String uri = view.getCursor().getURI();
        final String submitID = request.getState().getSubmitID();
        final Bundle bundle = view.getCursor().getXed().getBundle();
        // form submit buttons
        final Properties properties = request.getState().getProperties();
        final boolean isExpanded = Boolean.parseBoolean(properties.getProperty("buttons"));
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.DOCUMENT, qname, uri);
        final ActionButtons buttons = factory.create(null, getCursorActions(isExpanded, view));
        final Element tr = ElementU.addElement(tbody, Html.TR, null, NameTypeValuesU.create(Html.CLASS, Const.FOOTER));
        final Element th = ElementU.addElement(tr, Html.TD, null, NameTypeValuesU.create(
                Html.COLSPAN, Integer.toString(2), Html.CLASS, Const.DIALOG));
        for (final ActionButton button : buttons.getButtons()) {
            final SubmitToken tokenAction = new SubmitToken(
                    button.getSubject(), button.getAction(), button.getObject(), button.getObject2());
            HtmlU.addButton(th, button.getLabel(), buttons.getSubmitID(), tokenAction.toString(), null, null);
        }
        final Element span = ElementU.addElement(th, Html.SPAN);  // so buttons and expand/collapse on one line
        final String buttonToggle = (isExpanded ? UTF16.LIST_COLLAPSE : UTF16.LIST_EXPAND);
        ElementU.addElement(span, Html.A, buttonToggle, NameTypeValuesU.create(
                Html.HREF, "?toggle=buttons", Html.CLASS, "trailing"));
    }

    private static Collection<String> getCursorActions(final boolean isExpanded, final XedPropertyPageView view) {
        final XedCursor cursor = view.getCursor();
        //final XedCursor cursorParentC = cursor.getParentConcrete();
        final Node node = cursor.getNode();
        final Node parentNode = ((node == null) ? null : node.getParentNode());
        final boolean hasNode = (node != null);
        final boolean hasElementParent = (parentNode instanceof Element);
        final Collection<String> actions = new ArrayList<String>();
        add(actions, App.Action.CREATE, (!hasNode));
        add(actions, App.Action.UPDATE, hasNode);
        if (isExpanded) {
            add(actions, App.Action.DELETE, (hasNode && hasElementParent));
            add(actions, App.Action.CLONE, (hasNode && hasElementParent));
            add(actions, App.Action.UP, (hasNode && hasElementParent));
            add(actions, App.Action.DOWN, (hasNode && hasElementParent));
            //add(actions, App.Action.FILL, hasNode);
            //add(actions, App.Action.PRUNE, hasNode);
            //add(actions, App.Action.CLIP_CLEAR, SystemU.isTrue());
            //add(actions, App.Action.CLIP_CUT, (hasNode && hasElementParent));
            //add(actions, App.Action.CLIP_COPY, (hasNode && hasElementParent));
            //add(actions, App.Action.CLIP_PASTE, (cursorParentC != null));
        }
        return actions;
    }

    private static void add(final Collection<String> actions, final String name, final boolean condition) {
        if (condition) {
            actions.add(name);
        }
    }

    private static class Const {
        private static final String DIALOG = "dialog";
        private static final String HEADER = "header";
        private static final String FOOTER = "footer";
    }
}
