package io.github.greyp9.arwo.app.local.sh2.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.local.sh2.menu.MenuFavSH;
import io.github.greyp9.arwo.app.local.sh2.view.demo.TextAreaLSH;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.exec.script.ScriptContext;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);

        final MenuItem menuFavorites = new MenuFavSH(httpRequest.getBaseURI(), userState).toMenuItem()
                .applyFrom(userState.getMenuSystemState());
        new MenuHtml(httpRequest, null, userState.getSubmitID(), STYLE_HOME)
                .addTo(header, false, "v", Collections.singletonList(menuFavorites));

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

        return new AppHtmlView(httpRequest, userState, title, null, null)
                .title(header)
                .alerts(header)
                .statusBar(footer)
                .appHtml(html)
                .toHttpResponse(html);
    }

    private static final String STYLE_HOME = "background-color: brown; color: white;";
}
