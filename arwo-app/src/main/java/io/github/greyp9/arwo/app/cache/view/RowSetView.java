package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.value.Value;
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
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final UserStateTable table = new UserStateTable(userState, null, httpRequest.getDate(), true);
        table.toTableView(rowSet).addContentTo(body);
        final String labelContext = Value.wrap("[", "]", "label");
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(), labelContext);
        final MenuSystem menuSystem = userState.getMenuSystem();
        final MenuContext menuContext = new MenuContext(menuSystem, Collections.emptyList());
        return new AppHtmlView(httpRequest, userState, appTitle, menuContext, "").fixup(html);
    }
}
