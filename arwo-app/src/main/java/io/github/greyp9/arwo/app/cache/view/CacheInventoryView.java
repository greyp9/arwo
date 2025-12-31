package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.app.cache.core.Cache;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu2.core.MenuSession;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.text.TextU;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;

public final class CacheInventoryView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final ResourceCache resourceCache;

    public CacheInventoryView(final ServletHttpRequest httpRequest,
                              final AppUserState userState,
                              final ResourceCache resourceCache) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.resourceCache = resourceCache;
    }

    public HttpResponse render() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);
        final UserStateTable table = new UserStateTable(userState, null, httpRequest.getDate());
        table.toTableView(getRowSetR(resourceCache.getRowSets())).addContentTo(content);
        table.toTableView(getRowSetF(resourceCache.getFiles())).addContentTo(content);
        table.toTableView(getRowSetO(resourceCache.getObjects())).addContentTo(content);

        final String labelContext = TextU.wrapBracket(httpRequest.getBaseURI());
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

    private static final String MENU_KEY = "/menu2/cache";
    private static final String STYLE_HOME = "background-color: brown; color: white;";

    private RowSet getRowSetR(final Iterator<Map.Entry<String, RowSet>> iterator) {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(App.Attr.SELECT, Types.DATALINK),
                new ColumnMetaData(App.Attr.NAME, Types.VARCHAR, true),
                new ColumnMetaData(App.Attr.TYPE, Types.VARCHAR),
                new ColumnMetaData(App.Attr.ATTR, Types.VARCHAR),
                new ColumnMetaData(App.Attr.MTIME, Types.TIMESTAMP),
                new ColumnMetaData(App.Attr.SIZE, Types.INTEGER),
        };
        final RowSetMetaData rowSetMetaData = new RowSetMetaData(Cache.TABLE_ID_ROWSETS, columns);
        final RowSet rowSet = new RowSet(rowSetMetaData, null, null);
        while (iterator.hasNext()) {
            final Map.Entry<String, RowSet> entry = iterator.next();
            final RowSet value = entry.getValue();
            final String href = PathU.toPath(resourceCache.getEndpoint(), Cache.CONTEXT_ROWSETS, entry.getKey());
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, App.Action.SELECT, href));
            insertRow.setNextColumn(entry.getKey());
            insertRow.setNextColumn(value.getMetaData().getID());
            insertRow.setNextColumn(value.getMetaData().columnNames().toString());
            insertRow.setNextColumn(value.getDate());
            insertRow.setNextColumn(value.getRows());
            rowSet.add(insertRow.getRow());
        }
        return rowSet;
    }

    private RowSet getRowSetF(final Iterator<Map.Entry<String, MetaFile>> iterator) {
        // unify 'CacheInventoryView:getRowSetF' and 'MetaFileInventoryView:render'?
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(App.Attr.SELECT, Types.DATALINK),
                new ColumnMetaData(App.Attr.NAME, Types.VARCHAR, true),
                new ColumnMetaData(App.Attr.TYPE, Types.VARCHAR),
                new ColumnMetaData(App.Attr.ENCODING, Types.VARCHAR),
                new ColumnMetaData(App.Attr.RESOURCE, Types.VARCHAR),
                new ColumnMetaData(App.Attr.ATTR, Types.VARCHAR),
                new ColumnMetaData(App.Attr.MTIME, Types.TIMESTAMP),
                new ColumnMetaData(App.Attr.SIZE, Types.INTEGER),
        };
        final RowSetMetaData rowSetMetaData = new RowSetMetaData(Cache.TABLE_ID_METAFILES, columns);
        final RowSet rowSet = new RowSet(rowSetMetaData, null, null);
        while (iterator.hasNext()) {
            final Map.Entry<String, MetaFile> entry = iterator.next();
            final MetaFile value = entry.getValue();
            final String href = PathU.toPath(resourceCache.getEndpoint(), Cache.CONTEXT_METAFILES, entry.getKey());
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, App.Action.SELECT, href));
            insertRow.setNextColumn(entry.getKey());
            insertRow.setNextColumn(value.getContentType());
            insertRow.setNextColumn(value.getContentEncoding());
            insertRow.setNextColumn(new FileX(value.getMetaData().getPath()).getFilename());
            insertRow.setNextColumn(value.getMetaData().getProperties().stringPropertyNames());
            insertRow.setNextColumn(new Date(value.getMetaData().getLastModified()));
            insertRow.setNextColumn(value.getMetaData().getLength());
            rowSet.add(insertRow.getRow());
        }
        return rowSet;
    }

    private RowSet getRowSetO(final Iterator<Map.Entry<String, Object>> iterator) {
        final ColumnMetaData[] columns = new ColumnMetaData[] {
                new ColumnMetaData(App.Settings.NAME, Types.VARCHAR, true),
                new ColumnMetaData(App.Settings.TYPE, Types.VARCHAR),
        };
        final RowSetMetaData rowSetMetaData = new RowSetMetaData(Cache.TABLE_ID_OBJECTS, columns);
        final RowSet rowSet = new RowSet(rowSetMetaData, null, null);
        while (iterator.hasNext()) {
            final Map.Entry<String, Object> entry = iterator.next();
            final InsertRow insertRow = new InsertRow(rowSet);
            insertRow.setNextColumn(entry.getKey());
            insertRow.setNextColumn(entry.getValue().getClass().getName());
            rowSet.add(insertRow.getRow());
        }
        return rowSet;
    }
}
