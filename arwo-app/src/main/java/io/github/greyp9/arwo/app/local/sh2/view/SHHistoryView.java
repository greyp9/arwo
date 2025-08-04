package io.github.greyp9.arwo.app.local.sh2.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.local.sh.favorite.FavoriteMenu;
import io.github.greyp9.arwo.app.local.sh2.view.demo.TextAreaLSH;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.exec.script.ScriptContext;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class SHHistoryView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String context;

    public SHHistoryView(final ServletHttpRequest httpRequest, final AppUserState userState, final String context) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.context = context;
    }

    public final HttpResponse doGetResponse() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        new FavoriteMenu(httpRequest, userState, AppMenuFactory.Const.COMMAND_STICKY).addContentTo(header);

        final String command = userState.getProperties().getProperty(App.Settings.COMMAND, "");
        new TextAreaLSH(httpRequest, userState).addTextArea(content, command);

        final Collection<ScriptContext> scripts = CollectionU.copy(new ArrayList<>(), userState.getLSH().getScripts());
        final boolean filterByContext = !Value.isEmpty(context);
        final Collection<ScriptContext> scriptsDisplay = filterByContext
                ? scripts.stream().filter(s -> Value.equal(context, s.getContext())).collect(Collectors.toList())
                : scripts;
        new TableScript(httpRequest.getDate(), httpRequest.getBaseURI(), scriptsDisplay, userState).addTable(content);

        final AppTitle title = AppTitle.Factory.getResourceLabel(
                httpRequest, userState.getBundle(), Value.wrap("[", "]", httpRequest.getContextPath()));
        final MenuSystem menuSystem = userState.getMenuSystem();
        final List<MenuItem> menuItems = Collections.singletonList(
                menuSystem.get(httpRequest.getServletPath(), AppMenuFactory.Const.DASHBOARD)
        );
        final MenuContext menuContext = new MenuContext(menuSystem, menuItems);
        new AppHtmlView(httpRequest, userState, title, menuContext, "").fixup(html);

/*
        final Preferences preferences = new Preferences(userState.getConfig());
        final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
        final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
        new AppHtml(httpRequest).fixup(html, title, iconColor, theme);
*/

        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }
}
