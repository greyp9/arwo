package io.github.greyp9.arwo.app.jdbc.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.jdbc.sh.core.JDBCRequest;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu2.core.MenuSession;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;
import java.util.Optional;

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.ExcessiveImports" })
public abstract class JDBCView {
    private final JDBCRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final AppTitle title;

    public final JDBCRequest getRequest() {
        return request;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public JDBCView(final JDBCRequest request, final AppUserState userState) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;

        final String labelContext = Value.wrap("[", "]", request.getTitlePath());
        this.title = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(), labelContext);
    }

    public final HttpResponse doGetResponse() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);

        final MenuItem menu = new MenuSession().toMenu(MENU_KEY).applyFrom(userState.getMenuSystemState());
        final MenuHtml menuHtml = new MenuHtml(httpRequest, userState.getBundle(), userState.getSubmitID(), STYLE_HOME);
        menuHtml.addTo(header, true, "m", Collections.singletonList(menu));

        final HttpResponse httpResponse = addContentTo(content);
        return Optional.ofNullable(httpResponse).orElse(
                new AppHtmlView(httpRequest, userState, title)
                        .title(header)
                        .alerts(header)
                        .statusBar(footer)
                        .appHtml(html)
                        .toHttpResponse(html));
    }

    protected abstract HttpResponse addContentTo(Element html) throws IOException;

    private static final String MENU_KEY = "/menu2/jdbc";
    private static final String STYLE_HOME = "background-color: brown; color: white;";
}
