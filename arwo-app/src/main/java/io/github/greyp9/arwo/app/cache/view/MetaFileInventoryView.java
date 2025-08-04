package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public final class MetaFileInventoryView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final ResourceCache cache;

    public MetaFileInventoryView(final ServletHttpRequest httpRequest,
                                 final AppUserState userState,
                                 final ResourceCache cache) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.cache = cache;
    }

    public HttpResponse render() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Iterator<Map.Entry<String, MetaFile>> files = cache.getFiles();
        final RowSet rowSet = new MetaFileRowSet(
                "cacheType", userState.getSubmitID(), "/arwo/cache/f", files).getRowSet();
        final UserStateTable table = new UserStateTable(userState, null, httpRequest.getDate());
        table.toTableView(rowSet).addContentTo(body);
        final String labelContext = Value.wrap("[", "]", httpRequest.getServletPath());
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(), labelContext);
        final MenuSystem menuSystem = userState.getMenuSystem();
        final List<MenuItem> menuItems = Collections.singletonList(
                menuSystem.get(httpRequest.getServletPath(), AppMenuFactory.Const.DASHBOARD)
        );
        final MenuContext menuContext = new MenuContext(menuSystem, menuItems);
        return new AppHtmlView(httpRequest, userState, appTitle, menuContext, "").fixup(html);
    }
}
