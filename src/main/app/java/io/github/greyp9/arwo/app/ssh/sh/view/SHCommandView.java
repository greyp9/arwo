package io.github.greyp9.arwo.app.ssh.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.subsystem.ssh.SubsystemSSH;
import io.github.greyp9.arwo.app.core.view.history.AppHistoryView;
import io.github.greyp9.arwo.app.core.view.script.AppScriptView;
import io.github.greyp9.arwo.app.ssh.sh.core.SHRequest;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyPageHtmlView;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

import java.io.IOException;

public class SHCommandView extends SHView {
    private final SubsystemSSH ssh;

    public SHCommandView(final SHRequest request, final AppUserState userState) {
        super(request, userState);
        this.ssh = userState.getSSH();
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final SHRequest request = getRequest();
        final String scriptID = request.getScriptID();
        final Script script = ssh.getHistory().find(scriptID);
        // if command id is not in the list of cached commands, redirect to clear command id from URL
        final boolean badReference = ((!Value.isEmpty(scriptID)) && (script == null));
        return (badReference ?
                HttpResponseU.to302(PathU.toDir(request.getHttpRequest().getBaseURI(), request.getServer())) :
                addContentTo(html, script));
    }

    private HttpResponse addContentTo(final Element html, final Script script) throws IOException {
        final SHRequest request = getRequest();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        // command input form (prep)
        final String commandLast = ssh.getProperties().getProperty("command", "");
        final String commandText = (script == null) ? commandLast : script.getText();
        final XedActionCommand action = new XedActionCommand(userState.getLocus().getLocale());
        final Bundle bundle = action.getXed().getBundle();
        final NameTypeValues ntv = NameTypeValuesU.create("command.commandType.command", commandText);
        final XedCursor cursor = action.getCursor();
        final ValueInstance valueInstanceIn = ValueInstance.create(cursor.getTypeInstance(), ntv);
        new OpUpdate(null, action.getXed().getXsdTypes()).apply(cursor.getElement(), valueInstanceIn);
        // command input form
        final String qname = cursor.getTypeInstance().getQName().toString();
        final String submitID = userState.getSubmitID();
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.SESSION, qname, null);
        final ActionButtons buttons = factory.create(
                App.Action.COMMAND, false, CollectionU.toCollection(App.Action.COMMAND));
        final XedRequest xedRequest = new XedRequest(httpRequest, null, userState.getDocumentState());
        new PropertyPageHtmlView(new XedPropertyPageView(null, cursor, buttons), xedRequest).addContentTo(html);
        // contextual content
        if (script == null) {
            new AppHistoryView("sshHistoryType", ssh.getHistory(), bundle, httpRequest, userState).addContentTo(html);
        } else {
            new AppScriptView(script).addContentTo(html);
        }
        return null;
    }
}