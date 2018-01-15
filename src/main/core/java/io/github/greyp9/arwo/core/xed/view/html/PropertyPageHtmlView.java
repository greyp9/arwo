package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.action.ActionButton;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.HtmlU;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.core.XedU;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.extension.XedKey;
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

@SuppressWarnings({ "PMD.ExcessiveImports", "PMD.TooManyMethods" })
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
        final Element divDialog = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.DIALOG));
        final String formID = String.format("form_%s", cursorTypeName);
        final Element form = ElementU.addElement(divDialog, Html.FORM, null, NTV.create(
                Html.ACTION, Html.EMPTY, Html.ID, formID, Html.METHOD, Html.POST));
        // form table (for name / value alignment)
        final Element table = ElementU.addElement(form, Html.TABLE, null, NTV.create(
                Html.CLASS, App.CSS.DIALOG, Html.SUMMARY, App.CSS.DIALOG));
        final Element thead = ElementU.addElement(table, Html.THEAD, null, NTV.create(Html.CLASS, App.CSS.DIALOG));
        final Element trHead = ElementU.addElement(thead, Html.TR, null, NTV.create(Html.CLASS, App.CSS.HEADER));
        final Element th = ElementU.addElement(trHead, Html.TH, null, NTV.create(
                Html.COLSPAN, Integer.toString(2), Html.CLASS, App.CSS.HEADER));
        final String nameI18n = xsdBundle.getLabel(view.getCursor().getTypeInstance());
        ElementU.addElement(th, Html.SPAN, nameI18n, NTV.create(Html.CLASS, App.CSS.HEADER));
        // form table body
        final Element tbody = ElementU.addElement(table, Html.TBODY, null, NTV.create(Html.CLASS, App.CSS.DIALOG));
        // form fields correspond to leaf TypeInstances
        final Collection<ViewInstance> viewInstances = view.getViewInstances();
        if (viewInstances.isEmpty()) {
            final Element tr = ElementU.addElement(tbody, Html.TR);
            ElementU.addElement(tr, Html.TD, null, NTV.create(Html.CLASS, App.CSS.EMPTY));
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
        final boolean hideName = Boolean.parseBoolean(viewInstance.getTypeInstance().getDirective(XedU.HIDE_NAME));
        if (!hideName) {
            final String nameI18n = xsdBundle.getLabel(
                    view.getCursor().getTypeInstance(), viewInstance.getTypeInstance());
            ElementU.addElement(tr, Html.TD, nameI18n, NTV.create(Html.CLASS, App.CSS.ATTR_NAME));
        }
        addViewInstanceValue(viewInstance, tr);
    }

    private void addViewInstanceValue(final ViewInstance viewInstance, final Element tr) {
        final Element td = ElementU.addElement(tr, Html.TD, null, NTV.create(Html.CLASS, App.CSS.ATTR_VALUE));
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

    private void addViewInstanceValueDrillDown(final ViewInstanceDrillDown viewInstance, final Element td) {
        new DrillDownHtmlView(viewInstance).addContentTo(td);
    }

    private void addViewInstanceValueBoolean(final ViewInstanceBoolean viewInstance, final Element td) {
        new BooleanHtmlView(viewInstance).addContentTo(td);
    }

    private void addViewInstanceValueEnum(final ViewInstanceEnum viewInstance, final Element td) {
        new EnumHtmlView(viewInstance).addContentTo(td);
    }

    private void addViewInstanceValueChoice(final ViewInstanceChoice viewInstance, final Element td) {
        new ChoiceHtmlView(viewInstance).addContentTo(td);
    }

    private void addViewInstanceValueTextArea(final ViewInstanceTextArea viewInstance, final Element td) {
        new TextAreaHtmlView(viewInstance).addContentTo(td);
    }

    private void addViewInstanceValueTextMasked(final ViewInstanceTextMasked viewInstance, final Element td) {
        new TextMaskedHtmlView(viewInstance).addContentTo(td);
        if (PropertiesU.isBoolean(request.getState().getProperties(), App.Action.REVEAL)) {
            revealViewInstanceValueTextMasked(viewInstance, td);
        }
    }

    private void revealViewInstanceValueTextMasked(final ViewInstance viewInstance, final Element td) {
        try {
            final KeyX key = XedKey.getKeyPBE(request.getSecret(), viewInstance.getTypeInstance());
            final String clear = key.unprotect(viewInstance.getValue());
            ElementU.addElementNullable(td, Html.SPAN, Value.defaultOnEmpty(clear, null));
        } catch (IOException e) {
            request.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
    }

    private void addViewInstanceValueText(final ViewInstanceText viewInstance, final Element td) {
        new TextHtmlView(viewInstance).addContentTo(td);
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void addActions(final Element tbody) {
        final Properties properties = request.getState().getProperties();
        final boolean isExpanded = Boolean.parseBoolean(properties.getProperty(App.CSS.BUTTONS));
        final ActionButtons buttons = ((view.getButtons() == null) ? getActionButtons(isExpanded) : view.getButtons());
        final Element tr = ElementU.addElement(tbody, Html.TR, null, NTV.create(Html.CLASS, App.CSS.FOOTER));
        final Element th = ElementU.addElement(tr, Html.TD, null, NTV.create(
                Html.COLSPAN, Integer.toString(2), Html.CLASS, App.CSS.DIALOG));
        for (final ActionButton button : buttons.getButtons()) {
            final SubmitToken token = new SubmitToken(
                    button.getSubject(), button.getAction(), button.getObject(), button.getObject2());
            HtmlU.addButton(th, button.getLabel(), buttons.getSubmitID(), token.toString(),
                    null, button.getTitle(), Html.VALUE_1);
        }
        if (buttons.isExpander()) {
            final Element span = ElementU.addElement(th, Html.SPAN);  // so buttons and expand/collapse on one line
            final String buttonToggle = (isExpanded ? UTF16.LIST_COLLAPSE : UTF16.LIST_EXPAND);
            ElementU.addElement(span, Html.A, buttonToggle, NTV.create(
                    Html.HREF, "?toggle=buttons", Html.CLASS, App.CSS.TRAILING));  // i18n http
        }
    }

    private ActionButtons getActionButtons(final boolean isExpanded) {
        final String qname = view.getCursor().getTypeInstance().getQName().toString();
        final String uri = view.getCursor().getURI();
        final String submitID = request.getState().getSubmitID();
        final Bundle bundle = view.getCursor().getXed().getBundle();
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.DOCUMENT, qname, uri);
        return factory.create(null, true, getCursorActions(isExpanded, view));
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
}
