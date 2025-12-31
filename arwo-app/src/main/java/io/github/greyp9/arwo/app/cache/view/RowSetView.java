package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu2.core.MenuSession;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.text.TextU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;

public final class RowSetView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final RowSet rowSet;

    public RowSetView(final ServletHttpRequest httpRequest,
                      final AppUserState userState,
                      final RowSet rowSet) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.rowSet = rowSet;
    }

    public HttpResponse render() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);
        final UserStateTable table = new UserStateTable(userState, null, httpRequest.getDate());
        table.toTableView(rowSet).addContentTo(content);
        final String labelContext = TextU.wrapBracket(rowSet.getMetaData().getID());
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(), labelContext);
        addMenus(header);
        return new AppHtmlView(httpRequest, userState, appTitle, null, null)
                .title(header)
                .alerts(header)
                .actionLocale(header)
                .statusBar(footer)
                .appHtml(html)
                .toHttpResponse(html);
    }

    private void addMenus(final Element header) {
        final MenuItem menu = new MenuItem(UTF16.MENU, App.Target.USER_STATE, App.Action.MENU2, MENU_KEY, null,
                new MenuSession().toMenuItem(PathU.toPath(MENU_KEY, App.Target.SESSION)))
                .applyFrom(userState.getMenuSystemState());
        new MenuHtml(httpRequest, userState.getBundle(), userState.getSubmitID(), STYLE_HOME)
                .addTo(header, true, "m", Collections.singletonList(menu));
    }

    private static final String MENU_KEY = "/menu2/rowset";
    private static final String STYLE_HOME = "background-color: brown; color: white;";
}
