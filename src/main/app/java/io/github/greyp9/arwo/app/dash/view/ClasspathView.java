package io.github.greyp9.arwo.app.dash.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.table.core.TableU;
import io.github.greyp9.arwo.core.table.html.TableView;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.model.Table;
import io.github.greyp9.arwo.core.table.model.TableContext;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.url.URLCodec;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionFilter;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.Types;
import java.util.Date;

@SuppressWarnings("FieldCanBeLocal")
public class ClasspathView {
    private final AppRequest request;
    private final AppUserState userState;
    private final URLClassLoader urlClassLoader;

    public ClasspathView(final ServletHttpRequest httpRequest, final AppUserState userState, final Class<?> c) {
        this.request = userState.getAppRequest(httpRequest);
        this.userState = userState;
        this.urlClassLoader = Value.as(c.getClassLoader(), URLClassLoader.class);
    }

    public final void addContent(final Element html) throws IOException {
        if (urlClassLoader != null) {
            final RowSetMetaData metaData = createMetaData();
            final RowSet rowSet = createRowSet(metaData);
            final Bundle bundle = request.getBundle();
            final Locus locus = request.getLocus();
            //final String submitID = request.getSubmitID();
            final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
            final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
            TableU.addFooterStandard(table, bundle);
            final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
            final TableContext tableContext = new TableContext(
                    viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, locus);
            final TableView tableView = new TableView(table, tableContext);
            tableView.addContentTo(html);
        }
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData("path", Types.VARCHAR, true),  // i18n metadata
                new ColumnMetaData("mtime", Types.TIMESTAMP),  // i18n metadata
                new ColumnMetaData("size", Types.BIGINT),  // i18n metadata
        };
        return new RowSetMetaData("classloaderType", columns);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final URL[] urls = urlClassLoader.getURLs();
        if (urls.length > 0) {
            final URL firstUrl = urls[0];
            final File folderBase = URLCodec.toFile(firstUrl).getParentFile();
            for (URL url : urls) {
                final File file = URLCodec.toFile(url);
                createRow(rowSet, file, folderBase);
            }
        }
        return rowSet;
    }

    private void createRow(final RowSet rowSet, final File file, final File fileBase) {
        final String relativePath = file.getPath().replace(fileBase.getPath(), "");
        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(relativePath);
        insertRow.setNextColumn(new Date(file.lastModified()));
        insertRow.setNextColumn(file.length());
        rowSet.add(insertRow.getRow());
    }
}
