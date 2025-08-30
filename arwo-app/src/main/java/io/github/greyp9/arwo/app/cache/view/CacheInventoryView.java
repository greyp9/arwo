package io.github.greyp9.arwo.app.cache.view;

import io.github.greyp9.arwo.app.cache.core.Cache;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.MenuSystem;
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
import java.util.List;
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
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final UserStateTable table = new UserStateTable(userState, null, httpRequest.getDate());

        table.toTableView(getRowSetR(resourceCache.getRowSets())).addContentTo(body);
        table.toTableView(getRowSetF(resourceCache.getFiles())).addContentTo(body);
        table.toTableView(getRowSetO(resourceCache.getObjects())).addContentTo(body);

        final String labelContext = TextU.wrapBracket(httpRequest.getBaseURI());
        final AppTitle appTitle = AppTitle.Factory.getResourceLabel(httpRequest, userState.getBundle(), labelContext);
        final MenuSystem menuSystem = userState.getMenuSystem();
        final List<MenuItem> menuItems = Collections.singletonList(
                menuSystem.get(httpRequest.getServletPath(), AppMenuFactory.Const.DASHBOARD)
        );
        final MenuContext menuContext = new MenuContext(menuSystem, menuItems);
        return new AppHtmlView(httpRequest, userState, appTitle, menuContext, App.Token.EMPTY).fixup(html);
    }

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
