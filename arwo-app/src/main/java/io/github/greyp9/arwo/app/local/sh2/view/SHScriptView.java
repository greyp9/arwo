package io.github.greyp9.arwo.app.local.sh2.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.command.AppCommandView;
import io.github.greyp9.arwo.app.local.sh.favorite.FavoriteMenu;
import io.github.greyp9.arwo.app.local.sh2.view.demo.TextAreaLSH;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.exec.script.ScriptContext;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.xed.action.XedAction;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.model.XedFactory;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyStripHtmlView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;

public class SHScriptView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    //private final String context;
    private final String scriptID;

    public SHScriptView(final ServletHttpRequest httpRequest, final AppUserState userState, final String scriptID) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        //this.context = context;
        this.scriptID = scriptID;
    }

    public final HttpResponse doGetResponse() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);

        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        //final Element content = new XPather(document, null).getElement(Html.XPath.CONTENT);
        new FavoriteMenu(httpRequest, userState, AppMenuFactory.Const.COMMAND_STICKY).addContentTo(header);

        final String command = userState.getProperties().getProperty(App.Settings.COMMAND, "");
        new TextAreaLSH(httpRequest, userState).addTextArea(body, command);

        // stdin
        final XedAction actionStdin = new XedAction(new QName(App.Actions.URI_ACTION, App.Action.STDIN,
                App.Actions.PREFIX_ACTION), new XedFactory(), Locale.getDefault());
        final Xed xedUIStdin = actionStdin.getXedUI(actionStdin.getXed().getLocale());
        final XedPropertyPageView pageViewStdin = new XedPropertyPageView(null, new XedNav(xedUIStdin).getRoot());
        final Bundle bundleXed = xedUIStdin.getBundle();
        final ActionFactory factoryStdin = new ActionFactory(
                userState.getSubmitID(), bundleXed, App.Target.SESSION, App.Action.STDIN, null);
        final Collection<String> actionsStdin =
                io.github.greyp9.arwo.core.util.CollectionU.toCollection(App.Action.STDIN, App.Action.SIGNAL);
        final ActionButtons buttonsStdin = factoryStdin.create(App.Action.STDIN, false, actionsStdin);
        new PropertyStripHtmlView(pageViewStdin, buttonsStdin).addContentDiv(body);

        // history
        final Collection<ScriptContext> scripts = CollectionU.copy(new ArrayList<>(), userState.getLSH().getScripts());
        final Element divContent = ElementU.addElement(body, Html.DIV, null, NTV.create(Html.CLASS, "content"));
        final Date dateSubmit = DateX.Factory.createFilenameMilli().toDate(scriptID);
        final ScriptContext script = scripts.stream()
                .filter(s -> s.getDateSubmit().equals(dateSubmit)).findFirst().orElse(null);
        if (script != null) {
            new AppCommandView(script, userState.getLocus()).addContentTo(divContent);
        }

        final AppTitle title = AppTitle.Factory.getResourceLabel(
                httpRequest, userState.getBundle(), Value.wrap("[", "]", httpRequest.getContextPath()));
        final Preferences preferences = new Preferences(userState.getConfig());
        final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
        final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
        new AppHtml(httpRequest).fixup(html, title, iconColor, theme);

        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }
}
