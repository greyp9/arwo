package io.github.greyp9.arwo.app.local.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.subsystem.local.SubsystemLocal;
import io.github.greyp9.arwo.app.core.view.history.AppHistoryView;
import io.github.greyp9.arwo.app.core.view.script.AppScriptView;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.app.local.sh.favorite.FavoriteMenu;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.History;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedAction;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyPageHtmlView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import io.github.greyp9.arwo.core.xpath.XPather;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.stream.Collectors;

@SuppressWarnings("PMD.ExcessiveImports")
public class SHCommandView extends SHView {
    private final SubsystemLocal local;

    public SHCommandView(final SHRequest request, final AppUserState userState) {
        super(request, userState);
        this.local = userState.getLocal();
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final SHRequest request = getRequest();
        final String scriptID = request.getScriptID();
        final Script script = local.getHistory().find(scriptID);
        // if command id is not in the list of cached commands, redirect to clear command id from URL
        final boolean badReference = ((!Value.isEmpty(scriptID)) && (script == null));
        return (badReference
                ? HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getBaseURI()))
                : addContentTo(html, script));
    }

    private HttpResponse addContentTo(final Element html, final Script script) throws IOException {
        final Document document = html.getOwnerDocument();
        final Element header = new XPather(document, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(document, null).getElement(Html.XPath.CONTENT);
        final SHRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        final Properties properties = userState.getProperties();
        new FavoriteMenu(httpRequest, getUserState(), AppMenuFactory.Const.COMMAND_STICKY).addContentTo(header);
        // command input form (prep)
        final String command = (script == null) ? properties.getProperty(App.Settings.COMMAND, "") : script.getText();
        properties.setProperty(App.Settings.COMMAND, command);
        final XedActionCommand action = new XedActionCommand(userState.getXedFactory());
        final Xed xedUI = action.getXedUI(userState.getLocale());
        final XedCursor cursor = new XedNav(xedUI).getRoot();
        final Bundle bundle = cursor.getXed().getBundle();
        final NameTypeValues ntv = NameTypeValuesU.create("command.commandType.command", command);
        final ValueInstance valueInstanceIn = ValueInstance.create(cursor.getTypeInstance(), ntv);
        new OpUpdate(null, action.getXed()).apply(cursor.getElement(), valueInstanceIn);
        // command input form
        final String qname = cursor.getTypeInstance().getQName().toString();
        final String submitID = userState.getSubmitID();
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.SESSION, qname, null);
        final List<String> actions = new ArrayList<>(Collections.singleton(App.Action.COMMAND));
        final boolean notStarted = ((script != null) && (script.getStart() == null));
        if (notStarted) {
            actions.add(App.Action.CANCEL);
        }
        final ActionButtons buttons = factory.create(App.Action.COMMAND, false, actions);
        final XedRequest xedRequest = new XedRequest(httpRequest, null, userState.getDocumentState());
        new PropertyPageHtmlView(new XedPropertyPageView(null, cursor, buttons), xedRequest).addContentTo(content);
        // stdin
        final XedAction actionStdin = new XedAction(new QName(App.Actions.URI_ACTION, App.Action.STDIN,
                App.Actions.PREFIX_ACTION), new XedFactory(), Locale.getDefault());
        final Xed xedUIStdin = actionStdin.getXedUI(actionStdin.getXed().getLocale());
        final XedPropertyPageView pageViewStdin = new XedPropertyPageView(null, new XedNav(xedUIStdin).getRoot());
        final Bundle bundleXed = xedUIStdin.getBundle();
        final ActionFactory factoryStdin = new ActionFactory(
                userState.getSubmitID(), bundleXed, App.Target.SESSION, App.Action.STDIN, null);
        final Collection<String> actionsStdin = CollectionU.toCollection(App.Action.STDIN);
        final ActionButtons buttonsStdin = factoryStdin.create(App.Action.STDIN, false, actionsStdin);
        new PropertyStripHtmlView(pageViewStdin, buttonsStdin).addContentDiv(content);
        // contextual content
        if (script == null) {
            final String context = request.getContext();
            final boolean filterByContext = !Value.isEmpty(context);
            final History history = local.getHistory();
            final Collection<Script> scripts = history.getHistory();
            final Collection<Script> scriptsDisplay = filterByContext
                    ? scripts.stream().filter(s -> Value.equal(context, s.getContext())).collect(Collectors.toList())
                    : scripts;
            new AppHistoryView("lshHistoryType", true, scriptsDisplay, bundle,  // i18n metadata
                    httpRequest, userState).addContentTo(content);
        } else {
            new AppScriptView(script, userState.getLocus()).addContentTo(content);
        }
        return null;
    }
}
